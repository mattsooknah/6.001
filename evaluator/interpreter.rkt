;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; THE INTERPRETER
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "eval.rkt")

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

;; the read-eval-print loop
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input '**quit**)
        'meval-done
        (let ((output (m-eval input the-global-environment)))
          (announce-output output-prompt)
          (display output)
          (driver-loop)))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define *meval-warn-define* #t) ; print warnings?
(define *in-meval* #f)          ; evaluator running

;; the initial environment
(define (setup-environment)
  (let ((initial-env (extend-environment no-creator-procedure
                                         (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment))
        (oldwarn *meval-warn-define*))
    (set! *meval-warn-define* #f)
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    (set! *meval-warn-define* oldwarn)
    initial-env))

(define the-global-environment (setup-environment))
(define GE the-global-environment)

(define (refresh-global-environment)
  (set! the-global-environment (setup-environment))
  'done)

;; testing of advice implementation

(define around-nesting-test
  '(begin
     (define (square x) (* x x))
     (around (square x) (+ (square x) 1))
     (around (square x) (+ (square x) 1))
     (square 5)))

(define advice-combo-test
  '(begin
     (define (fact n) 
       (if (= n 0) 
           1 
           (* n (fact (- n 1)))))
     (after (fact n output)
       (display "fact ")
       (display n)
       (display " returned ")
       (display output)
       (newline))
     (around (fact n) 
       (if (< n 0) 
           (* (fact (- n)) (if (even? n) 1 -1)) 
           (fact n)))
     (before (fact n) 
       (if (even? n) 
           (display 'tick) 
           (display 'tock)) 
       (newline))
     (fact -4)))

(define constraint-test
  '(begin
     (define (fib n)
       (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
     (define count 0)
     (before (+ (in fib) x y)
       (set! count (+ count 1)))
     (fib 5)
     count))

(m-eval around-nesting-test GE)
(m-eval advice-combo-test GE)
(m-eval constraint-test GE)

;; initializing the interpreter
(driver-loop)