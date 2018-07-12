#lang racket/base
(require racket/cmdline
         racket/system
         racket/path
         racket/string
         racket/set)

(define whoosh "whoosh")

(define fail? #f)
(define try-count 0)
(define fail-count 0)
(define results null)

(define output-to #f)
(define output-label #f)

(define s-printf printf)

(define clean? #f)

(define script-file-or-dir-list
  (command-line
   #:once-each
   [("--program") whoosh-executable "Select a `whoosh` executable"
    (set! whoosh whoosh-executable)]
   [("--results") label dest-file "Record results"
    (set! output-to dest-file)
    (set! output-label label)]
   [("-e") "Print status output to stderr"
    (set! s-printf (lambda args
		     (apply eprintf args)
		     (apply printf args)))]
   [("--clean") "Clean up lingering processes"
    (set! clean? #t)]
   #:args
   script-or-dir
   script-or-dir))

(define (get-actions f)
  (call-with-input-file*
   f
   (lambda (i)
     (cond
      [(regexp-try-match #rx"# Needed input:\n" i)
       (define lines (get-lines i null cons cons))
       (define steps
         (for/list ([in (in-list lines)])
           (cond
             [(equal? in "ctl-c")
              (lambda (p i)
                (unless (eq? 'running (p 'status))
                  (set! fail? #t)
                  (eprintf " FINISHED TOO EARLY\n"))
                (p 'interrupt))]
             [(regexp-match #rx"^sleep ([0-9]+(?:[.][0-9]*)?)$" in)
              => (lambda (m)
                   (lambda (p i)
                     (sleep (string->number (cadr m)))))]
             [else
              (error 'get-action "unknown input-step: ~s" in)])))
       (lambda (p i)
         (for ([step (in-list steps)])
           (step p i)))]
      [else (lambda (p i) (void))]))))

(define (get-matcher f)
  (call-with-input-file*
   f
   (lambda (i)
     (define (get-output-lines)
       (get-lines i
                  ""
                  (lambda (l m) (string-append l "\n" m))
                  (lambda (l m) (string-append l m))))
     (cond
      [(regexp-try-match #rx"# Expected output:\n" i)
       (define lines (get-output-lines))
       (cond
        [(regexp-try-match #rx"^# OR\n" i)
         (define alt-lines (get-output-lines))
         (lambda (s)
           (or (string=? s lines)
               (string=? s alt-lines)))]
        [else
         (lambda (s) (string=? s lines))])]
      [(regexp-try-match #rx"# Expect output matching:\n" i)
       (define rx (pregexp (string-trim (get-output-lines))))
       (lambda (s) (regexp-match? rx s))]
      [else (error 'get-matcher
                   "could not find expected output in script\n  script: ~a"
                   f)]))))

(define (get-minimum-seconds f)
  (call-with-input-file*
   f
   (lambda (i)
     (cond
      [(regexp-try-match #rx"# Minimum seconds:\n" i)
       (define n (get-lines i "" string-append string-append))
       (define secs (string->number n))
       (unless secs
         (error 'get-minimum-seconds "base seconds specification: ~s" n))
       secs]
      [else
       0]))))

(define (get-lines i base combine combine-continue)
  (let loop ()
    (cond
     [(regexp-try-match #rx"^#  [^\n]*\n" i)
      => (lambda (m)
           (define line (bytes->string/utf-8 
                         (subbytes (car m)
                                   3
                                   (sub1 (bytes-length (car m))))))
           (define more (loop))
           (if (regexp-match? #rx"\\\\$" line)
               (combine-continue (substring line 0 (sub1 (string-length line))) more)
               (combine line more)))]
     [else base])))

(define (test-one-file f)
  (set! fail? #f)
  (s-printf "Trying ~a\n" f)
  (define input-actions (get-actions f))
  (define output-matcher (get-matcher f))
  (define min-secs (get-minimum-seconds f))
  (define o (open-output-bytes))
  (define e (open-output-bytes))
  (define c (make-custodian))
  (define started (current-seconds))
  (define-values (_o in pid _e p)
    (apply values
           (parameterize ([current-subprocess-custodian-mode 'kill]
                          [subprocess-group-enabled #t]
                          [current-custodian c])
             (process*/ports o #f e whoosh f))))
  (input-actions p in)
  (define th
    (parameterize ([current-custodian c])
      (thread (lambda () (p 'wait)))))
  (unless (sync/timeout 10 th)
    (set! fail? #t)
    (eprintf " TIMEOUT\n"))
  (custodian-shutdown-all c)
  (when ((- (current-seconds) started) . < . min-secs)
    (set! fail? #t)
    (eprintf " COMPLETED TOO QUICKLY %d\n"))
  (unless (equal? 0 (p 'exit-code))
    (eprintf " NON-ZERO EXIT CODE: ~s\n" (p 'status))
    (set! fail? #t))
  (cond
   [(output-matcher (get-output-string o))
    (s-printf " Passed output check\n")]
   [else
    (set! fail? #t)
    (eprintf " FAILED: ~s\n" (get-output-string o))])
  (unless (equal? "" (get-output-string e))
    (set! fail? #t)
    (eprintf " NON-EMPTY STDERR: ~s\n" (get-output-string e)))
  (set! try-count (+ 1 try-count))
  (set! fail-count (+ (if fail? 1 0) fail-count)))

(define (test-all-files script-file-or-dir)
  (cond
    [(directory-exists? script-file-or-dir)
     (for ([f (directory-list script-file-or-dir #:build? #t)])
       (when (or (directory-exists? f)
                 (path-has-extension? f #".whoosh"))
         (test-all-files f)))]
    [else
     (test-one-file script-file-or-dir)]))

;; ----------------------------------------

(define (get-pids)
  (define o (open-output-bytes))
  (define-values (no-i no-o ps-pid no-e ctl)
    (apply
     values
     (process*/ports o (current-input-port) (current-error-port)
		     "/bin/ps" "x")))
  (ctl 'wait)
  (for*/set ([s (in-lines (open-input-bytes (get-output-bytes o)))]
	     [pid (in-value (string->number
			     (cadr (or (regexp-match #rx"^ ([0-9]+)" s)
				       '("" "")))))]
	     #:when (and pid (not (= pid ps-pid))))
    pid))

(define pre-pids (and clean? (get-pids)))

(define total-try-count 0)
(define total-fail-count 0)

(for ([script-file-or-dir (in-list script-file-or-dir-list)])
  (set! try-count 0)
  (set! fail-count 0)
  (test-all-files script-file-or-dir)
  (set! results (list* try-count
                       (- try-count fail-count)
		       results))
  (set! total-try-count (+ total-try-count try-count))
  (set! total-fail-count (+ total-fail-count fail-count)))

(define post-pids (and clean? (get-pids)))
(when clean?
  (for ([pid (in-set post-pids)])
    (unless (set-member? pre-pids pid)
      (eprintf "Lingering process: ~a\n" pid)
      (system* "/bin/kill" "-9" (format "~a" pid)))))

(printf "Passed ~a/~a\n"
	(- total-try-count total-fail-count) 
	total-try-count)

(when output-to
  (call-with-output-file output-to
    #:exists 'append
    (lambda (o)
      (fprintf o "~a" output-label)
      (for ([r (in-list (reverse results))])
	(fprintf o "\t~a" r))
      (newline o))))

(when (positive? total-fail-count)
  (exit 1))
