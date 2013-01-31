;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; THE METACIRCULAR EVALUATOR
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "environment.rkt")
(load "expressions.rkt")
(load "primitives.rkt")

;; eval

(define (m-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))    
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((reset? exp) (eval-reset exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((before? exp) (eval-before exp env))
        ((around? exp) (eval-around exp env))
        ((after? exp) (m-eval (after->around exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
        ((let? exp) (m-eval (let->application exp) env))
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                  (list-of-values (operands exp) env) env))
        (else (error "Unknown expression type -- EVAL" exp))))

;; apply

(define (m-apply procedure arguments env)
  (cond ((has-advice? procedure)
         (if (constraint-satisfied? procedure env)
             (apply-advice procedure arguments env)
             (m-apply (procedure-part procedure) arguments env)))
        ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment procedure
                              (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (apply-advice advised-procedure arguments env)
  (let ((name (name-part advised-procedure))
        (advice (advice-part advised-procedure))
        (original-proc (procedure-part advised-procedure)))
    (cond ((before-procedure? advice)
           (begin
             (eval-sequence 
              (advice-body advice)
              (extend-environment advice
                                  (advice-parameters advice) 
                                  arguments 
                                  (advice-environment advice)))
             (m-apply original-proc arguments env)))
          ((around-procedure? advice)
           (eval-sequence
            (advice-body advice)
            (extend-environment advice
                                (cons name (advice-parameters advice))
                                (cons original-proc arguments)
                                (advice-environment advice))))
        (else (error "Unknown advice type -- APPLY" advice)))))

;; handling of special forms

(define (eval-before exp env)
  (add-advice! 
   (before-proc-name exp)
   (make-advice 'before-procedure (before-parameters exp) (before-body exp) env)
   (before-constraint exp)
   env))

(define (eval-around exp env)
  (add-advice! 
   (around-proc-name exp)
   (make-advice 'around-procedure (around-parameters exp) (around-body exp) env) 
   (around-constraint exp)
   env))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (m-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (m-eval (if-predicate exp) env)
      (m-eval (if-consequent exp) env)
      (m-eval (if-alternative exp) env)
      ))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (m-eval (first-exp exps) env))
        (else (m-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (m-eval (assignment-value exp) env)
                       env))

(define (eval-reset exp env)
  (reset-variable-value! (reset-variable exp) env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (m-eval (definition-value exp) env)
                    env))

;; desugaring

(define (after->around exp)
  (let* ((proc-name (after-proc-name exp))
         (constraint (after-constraint exp))
         (parameters (after-parameters exp))
         (result (after-result exp))
         (body (after-body exp)))
    (list 'around 
          (append (if (constrained? exp) (list proc-name constraint) (list proc-name)) parameters)
          (append (list 'let (list (list result (cons proc-name parameters)))) body (list result)))))
                
(define (let->application expr)
  (let ((names (let-bound-variables expr))
        (values (let-values expr))
        (body (let-body expr)))
    (make-application (make-lambda names body)
                      values)))

(define (cond->if expr)
  (let ((clauses (cond-clauses expr)))
    (if (null? clauses)
        #f
        (if (eq? (car (first-cond-clause clauses)) 'else)
            (make-begin (cdr (first-cond-clause clauses)))
            (make-if (car (first-cond-clause clauses))
                     (make-begin (cdr (first-cond-clause clauses)))
                     (make-cond (rest-cond-clauses clauses)))))))