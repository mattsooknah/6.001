;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The 6.001 Meta-Circular Evaluator 
;;;     (Controllably) Lazy Version
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   The Core Evaluator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (l-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
   ((cond? exp) (l-eval (cond->if exp) env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp) (lambda-body exp) env))
	((begin? exp) (eval-sequence (begin-actions exp) env))
	((application? exp)
	 (l-apply (actual-value (operator exp) env)
		  (operands exp)
		  env))
	(else (error "Unknown expression type -- L-EVAL" exp))))

(define (l-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (let ((params (procedure-parameters procedure)))
            (extend-environment (map parameter-name params)
                                (list-of-delayed-args params arguments env)
                                (procedure-environment procedure)))))
         (else (error "Unknown procedure type -- L-APPLY" procedure))))

;(trace l-eval l-apply)

(define (list-of-arg-values exps env)
  (if (no-operands? exps) '()
      (cons (actual-value (first-operand exps) env)
	    (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args var-decls exps env)
  (if (no-operands? exps) '()
      (cons (delay-it (first-variable var-decls) (first-operand exps) env)
            (list-of-delayed-args (rest-variables var-decls) (rest-operands exps) env))))

(define (eval-if exp env)
  (if (actual-value (if-predicate exp) env)
      (l-eval (if-consequent exp) env)
      (l-eval (if-alternative exp) env)))

(define (cond->if exp)
  (define (nested-if condlist)
    (if (null? condlist) #f
        (let ((predicate (caar condlist))
              (consequent (cadar condlist))
              (alternatives (cdr condlist)))
          (list 'if predicate consequent 
                (nested-if alternatives)))))
  (nested-if (cond-clauses exp)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (l-eval (first-exp exps) env))
	(else (l-eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (l-eval (assignment-value exp) env)
		       env))

(define (eval-definition exp env)
;  (print "start")
  (define-variable! (definition-variable exp)
    (l-eval (definition-value exp) env)
    env)
;  (print "end")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Representing Expressions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal params
		   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters lambda-exp) (cadr lambda-exp))

(define (lambda-body lambda-exp) (cddr lambda-exp))

(define (make-lambda parms body) (cons 'lambda (cons parms body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if pred conseq alt) (list 'if pred conseq alt))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representing procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? exp)
  (tagged-list? exp 'procedure))

(define (procedure-parameters p) (list-ref p 1))

(define (procedure-body p) (list-ref p 2))

(define (procedure-environment p) (list-ref p 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Controlling parameter lazyness
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (first-variable var-decls) (car var-decls))

(define (rest-variables var-decls) (cdr var-decls))

(define declaration? pair?)

(define (parameter-name var-decl)
  (if (pair? var-decl) (car var-decl) var-decl))

(define (lazy? var-decl)
  (and (pair? var-decl) (eq? 'lazy (cadr var-decl))))

(define (memo? var-decl)
  (and (pair? var-decl) (eq? 'lazy-memo (cadr var-decl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representing environments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implement environments as a list of frames; parent environment is
;; the cdr of the list. Each frame will be implemented as a list
;; of variables and a list of corresponding values.

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values) (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many args supplied" vars vals)
	  (error "Too few args supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- LOOKUP" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val)) ; Same as lookup except for this
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
	    ((eq? var (car vars)) (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Primitive Procedures and the Initial Environment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
   (list 'pair? pair?)
	(list 'list list)
	(list 'eq? eq?)
	(list '+ +)
	(list '> >)
   (list '< <)
	(list '= =)
	(list '* *)
	(list '- -)
   (list '/ /)
   (list 'expt expt)
   (list 'abs abs)
	; ... more primitives
	))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
					 (primitive-procedure-objects)
					 the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(define GE the-global-environment)

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Higher-Order Procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define or-val-def
   '(define or-val
      (lambda (e1 e2 e3)
        (if (eq? #f e1)
            (or-val e2 e3 #t)
            e1))))

(define append-def
  '(define append
     (lambda (lst1 lst2)
       (if (null? lst1) lst2
           (cons (car lst1) (append (cdr lst1) lst2))))))

(define inc-def
  '(define inc
     (lambda (x) (+ x 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The Read-Eval-Print Loop
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define input-prompt ";;; L-Eval input:")

(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (display output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Representing Thunks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (delay-it decl exp env)
  (cond ((not (declaration? decl))
         (l-eval exp env))
        ((lazy? decl)
         (list 'thunk exp env))
        ((memo? decl)
         (list 'thunk-memo exp env))
        (else (error "unknown declaration:" decl))))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((memoized-thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(define (actual-value exp env)
  (force-it (l-eval exp env)))

(define (thunk? obj) (tagged-list? obj 'thunk))
(define (memoized-thunk? obj) (tagged-list? obj 'thunk-memo))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Representing Streams (within the lazy interpreter)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stream-defs
  '(begin
          
     (define stream-cons
       (lambda (x (y lazy-memo))
         (lambda (msg)
           (cond ((eq? msg 'stream-car) x)
                 ((eq? msg 'stream-cdr) y)
                 (else (error "unknown stream msg" msg))))))
     
     (define (stream-car s) (s 'stream-car))
     (define (stream-cdr s) (s 'stream-cdr))
     
     (define cons-stream stream-cons) 
     
     (define the-empty-stream '())

     (define stream-interval
       (lambda (a b)
         (if (> a b)
             the-empty-stream
             (stream-cons a (stream-interval (+ a 1) b)))))
     
     (define stream-ref
       (lambda (str index)
         (if (= 0 index) (stream-car str)
             (stream-ref (stream-cdr str) (- index 1)))))
     
     (define print-stream
       (lambda (str n)
         (define print-iter
           (lambda (compiled-list remaining-str remaining-elts) 
             (if (= remaining-elts 0) compiled-list
                 (print-iter (append compiled-list (list (stream-car remaining-str)))
                             (stream-cdr remaining-str)
                             (- remaining-elts 1)))))
         (print-iter '() str n)))
     
     (define stream-print print-stream)
     
     (define interleave
       (lambda (s1 s2)
         (cons-stream
          (stream-car s1)
          (interleave s2 (stream-cdr s1)))))
     
     (define map-stream
       (lambda (proc str)
         (cons-stream
          (proc (stream-car str))
          (map-stream proc (stream-cdr str)))))

     (define map2-stream 
       (lambda (proc s1 s2)
         (cons-stream
          (proc (stream-car s1) (stream-car s2))
          (map2-stream proc (stream-cdr s1) (stream-cdr s2)))))
     
     (define filter-stream
       (lambda (pred str)
         (if (pred (stream-car str))
             (cons-stream (stream-car str) (filter-stream pred (stream-cdr str)))
             (filter-stream pred (stream-cdr str)))))
     
     (define reduce-stream-up-to-n
       (lambda (op str init n)
         (if (= n 0) init
             (op (stream-car str) 
                 (reduce-stream-up-to-n op (stream-cdr str) init (- n 1))))))
     
     (define ones (stream-cons 1 ones))
     (define twos (stream-cons 2 twos))
     (define ints (stream-cons 1 (map-stream inc ints)))
     (define the-facts (map2-stream * ints (cons-stream 1 the-facts)))
     
     (define fibs
       (lambda ()
         (cons-stream 1
          (cons-stream 1
           (map2-stream + (fibs) (stream-cdr (fibs)))))))
     
     (define powers
       (lambda (x)
         (define repeating-x (cons-stream x repeating-x))
         (map2-stream
          expt
          repeating-x
          (cons-stream 0 ints))))
          
     (define exponent
       (lambda (x)
         (define power-series
           (map2-stream / (powers x) (cons-stream 1 the-facts)))
         (map2-stream + power-series (cons-stream 0 (exponent x)))))
     
     (define gradient
       (lambda (str)
         (map2-stream - str (stream-cdr str))))
     
     (define threshold
       (lambda (str thresh)
         (map-stream (lambda (x) (if (> (abs x) thresh) 1 0)) str)))
     
     (define edges
       (lambda (str thresh)
         (define gradient-stream (gradient str))
         (define threshold-stream (threshold gradient-stream thresh))
         (define index-stream ints)
         (define unfiltered-edge-values
           (map2-stream (lambda (a b) (if (= a 1) b #f)) threshold-stream gradient-stream))
         (define unfiltered-edges
           (map2-stream (lambda (a b) (if a (list (abs a) b) #f)) unfiltered-edge-values index-stream))
         (filter-stream pair? unfiltered-edges)))
     
     (define flatten-stream
       (lambda (str)
         (define flatten-iter
           (lambda (unflattened-str)
             (cons-stream
              (stream-car (stream-car unflattened-str))
              (interleave 
               (stream-cdr (stream-car unflattened-str))
               (flatten-iter (stream-cdr unflattened-str))))))
         (flatten-iter str)))
       
     (define rationals
       (lambda ()
         (define all-pairings
           (lambda (s1 s2)
             (map-stream 
              (lambda (s1-entry) 
                (map-stream 
                 (lambda (s2-entry) 
                   (cons s1-entry s2-entry)) 
                 s2)) 
              s1)))
         (define int-pairs
           (lambda () (flatten-stream (all-pairings ints ints))))
         (map-stream (lambda (pair) (/ (car pair) (cdr pair))) (int-pairs))))
         
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Pre-emptive Evaluations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(actual-value or-val-def GE)
(actual-value append-def GE)
(actual-value inc-def GE)
(actual-value stream-defs GE)

#|

;; test of memoization (do not uncomment; copy into lazy interpreter)

(begin
(define count1 0)
(define count2 0)
(define count3 0)
(define foo1 (lambda (x) (* x x)))
(define foo2 (lambda ((x lazy)) (* x x)))
(define foo3 (lambda ((x lazy-memo)) (* x x)))
(define bar1 (lambda () (set! count1 (+ count1 1)) 10))
(define bar2 (lambda () (set! count2 (+ count2 1)) 10))
(define bar3 (lambda () (set! count3 (+ count3 1)) 10))
(foo1 (bar1))
(foo2 (bar2))
(foo3 (bar3))
(list count1 count2 count3)
)

;; expected output: (1 2 1)

|#

(driver-loop)