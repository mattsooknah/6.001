;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REPRESENTING EXPRESSIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define rest cdr)
(define middle
  (lambda (lst)
    (define middle-iter
      (lambda (read unread)
        (if (null? (cdr unread)) read
            (middle-iter 
             (append read (list (car unread)))
             (cdr unread)))))
    (middle-iter nil (cdr lst))))
(define last
  (lambda (lst)
    (if (null? (cdr lst)) (car lst)
        (last (cdr lst)))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp) (boolean? exp)))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-assignment var expr)
  (list 'set! var expr))

(define (reset? exp) (tagged-list? exp 'reset!))
(define (reset-variable exp) (cadr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))   (cadr exp)   (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))  ; formal params, body
(define (make-define var expr)
  (list 'define var expr))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp) (cddr lambda-exp))
(define (make-lambda parms body) (cons 'lambda (cons parms body)))

(define (before? exp) (tagged-list? exp 'before))
(define (around? exp) (tagged-list? exp 'around))
(define (after? exp) (tagged-list? exp 'after))
(define (constrained? exp) (pair? (second (cadr exp))))

(define (before-proc-name exp) (first (cadr exp)))
(define (before-constraint exp)
  (if (constrained? exp)
      (second (cadr exp))
      nil))
(define (before-parameters exp)
  (if (constrained? exp)
      (rest (rest (cadr exp)))
      (rest (cadr exp))))
(define (before-body exp) (cddr exp))

(define (around-proc-name exp) (first (cadr exp)))
(define (around-constraint exp)
  (if (constrained? exp)
      (second (cadr exp))
      nil))
(define (around-parameters exp)
  (if (constrained? exp)
      (rest (rest (cadr exp)))
      (rest (cadr exp))))
(define (around-body exp) (cddr exp))

(define (after-proc-name exp) (first (cadr exp)))
(define (after-constraint exp)
  (if (constrained? exp)
      (second (cadr exp))
      nil))
(define (after-parameters exp) 
  (if (constrained? exp)
      (middle (rest (cadr exp)))
      (middle (cadr exp))))
(define (after-result exp) (last (cadr exp)))
(define (after-body exp) (cddr exp))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp)) 
(define (if-consequent exp) (caddr exp)) 
(define (if-alternative exp) (cadddr exp))
(define (make-if pred conseq alt) (list 'if pred conseq alt))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define first-cond-clause car)
(define rest-cond-clauses cdr)
(define (make-cond seq) (cons 'cond seq))

(define (let? expr) (tagged-list? expr 'let))
(define (let-bound-variables expr) (map first (second expr)))
(define (let-values expr) (map second (second expr)))
(define (let-body expr) (cddr expr)) ;differs from lecture--body may be a sequence
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin exp) (cons 'begin exp))

(define (application? exp) (pair? exp))
(define (operator app) (car app))
(define (operands app) (cdr app))
(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))
(define (make-application rator rands)
  (cons rator rands))

(define (and? expr) (tagged-list? expr 'and))
(define and-exprs cdr)
(define (make-and exprs) (cons 'and exprs))
(define (or? expr) (tagged-list? expr 'or))
(define or-exprs cdr)
(define (make-or exprs) (cons 'or exprs))
(define (true? exp) (if exp #t #f))