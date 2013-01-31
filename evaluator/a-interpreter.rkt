;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ANALYZE INTERPRETER
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "a-eval.rkt")

(define input-prompt ";;; A-Eval input:")
(define output-prompt ";;; A-Eval value:")

;; the read-eval-print loop
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input '**quit**)
        'aeval-done
        (let ((output (a-eval input the-global-environment)))
          (announce-output output-prompt)
          (display output)
          (driver-loop)))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define *aeval-warn-define* #t) ; print warnings?
(define *in-aeval* #f)          ; evaluator running

;; the initial environment
(define (setup-environment)
  (let ((initial-env (extend-environment no-creator-procedure
                                         (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment))
        (oldwarn *aeval-warn-define*))
    (set! *aeval-warn-define* #f)
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    (set! *aeval-warn-define* oldwarn)
    initial-env))

(define the-global-environment (setup-environment))
(define GE the-global-environment)

(define (refresh-global-environment)
  (set! the-global-environment (setup-environment))
  'done)

;; testing

(define basic-test
  '(begin
     (define count 0)
     (define (thingy n)
       (if (= n 0) 
           count 
           (begin 
             (set! count (+ count 1)) 
             (thingy (- n 1)))))
     (thingy 5)))

(define let-test
  '(begin
     (define (max lst)
       (let ((first (car lst))
             (rest (cdr lst)))
         (cond ((null? rest) first)
               ((> first (max rest)) first)
               (else (max rest)))))
     (max '(1 2 3 4 3 2 1))))

;(a-eval basic-test GE)
;(a-eval let-test GE)

;; initialize the interpreter
(driver-loop)