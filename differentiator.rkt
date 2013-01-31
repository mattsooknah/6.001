#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;; 
;;  SYMBOLIC DIFFERENTIATION (SICP 2.3.2)  ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; differentiation can be reduced to a few simple rules
;; choosing the right rule requires us to determine
;; whether the expression being differentiated
;; is a sum, product, a variable, or a number

;; We begin with a simple rule set for doing algebra

#|

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (exponentiation? x)
  (and (pair? x) 
       (eq? (car x) '**) 
       (number? (eval (caddr x))))) ;; for simplicity, the exponent must be a number

(define (binary? x)  ;; tests whether an operation expression, e.g. '(+ a 1),
  (null? (cdddr x))) ;; is binary (i.e has two operands)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; sums and products must be implemented as symbolic
;; data structures, because the arguments may not be numbers -
;; they might be variables, or even sum/product expressions themselves.

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2) ;; first three clauses simplify 
        ((=number? a2 0) a1) ;; expressions containing numbers
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ (eval-sum a1) (eval-sum a2)))))

(define (addend s) (cadr s))
(define (augend s) 
  (if (binary? s) (caddr s)    ;; for binary sums, selects single augend
      (append '(+) (cddr s)))) ;; else groups augends into sub-sum expression

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2) ;; first four clauses simplify
        ((=number? m2 1) m1) ;; expressions containing numbers
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* (eval-product m1) (eval-product m2)))))

(define (multiplier p) (cadr p))
(define (multiplicand p) 
  (if (binary? p) (caddr p)    ;; for binary products, selects single multiplicand 
      (append '(*) (cddr p)))) ;; else groups multiplicands into sub-product expression

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((number? base) (expt base exponent))
        (else (list '** base exponent))))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (eval-sum s)
  (if (sum? s) (make-sum (addend s) (augend s)) s))
(define (eval-product p)
  (if (product? p) (make-product (multiplier p) (multiplicand p)) p))
(define (eval-exponentiation e)
  (if (exponentiation? e) (make-exponentiation (base e) (exponent e)) e))
(define (eval exp)
  (eval-sum (eval-product (eval-exponentiation exp))))

|#

;; since everything is just a symbol anyway, we should be able to use
;; any algebraic notation we want, then convert it to Scheme-friendly
;; notation internally. here is an alternative rule set for standard notation
;; (including order of operations)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (contains? symbol x)
  (if (memq symbol x) #t #f))
        
(define (sum? x)
  (and (pair? x) (contains? '+ x)))

(define (product? x)
  (and (pair? x) (contains? '* x) (not (sum? x))))

(define (exponentiation? x)
  (and (pair? x) 
       (contains? '** x)
       (not (product? x))
       (not (sum? x))
       (number? (eval (exponent x)))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; UNTIL returns everything in a sequence up to the first instance of the specified symbol
;; (or the whole sequence, if the symbol is not found)

(define (until symbol sequence)
  (define (until-iter symbol unchecked-segment checked-segment)
    (if (or (null? unchecked-segment)
            (eq? symbol (car unchecked-segment)))
        checked-segment
        (until-iter symbol (cdr unchecked-segment) 
                    (append checked-segment (list (car unchecked-segment))))))
  (until-iter symbol sequence empty))

;; BEYOND returns everything beyond the first instance of the specified symbol
;; (or empty, if symbol not found)

(define (beyond symbol sequence)
  (if (not (memq symbol sequence)) empty
      (cdr (memq symbol sequence))))

(define (make-sum a1 a2)
  (let ((a1 (eval a1))
        (a2 (eval a2)))
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list a1 '+ a2)))))

(define (addend s)
  (let ((until+ (until '+ s)))
    (if (null? (cdr until+)) (car until+) until+)))
(define (augend s)
  (let ((beyond+ (beyond '+ s)))
    (if (null? (cdr beyond+)) (car beyond+) beyond+)))

(define (make-product m1 m2)
  (let ((m1 (eval m1))
        (m2 (eval m2)))
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2)))))

(define (multiplier p)
  (let ((until* (until '* p)))
    (if (null? (cdr until*)) (car until*) until*)))
(define (multiplicand p)
  (let ((beyond* (beyond '* p)))
    (if (null? (cdr beyond*)) (car beyond*) beyond*)))

(define (make-exponentiation base exponent)
  (let ((base (eval base))
        (exponent (eval exponent)))
    (cond ((= exponent 0) 1)
          ((= exponent 1) base)
          ((number? base) (expt base exponent))
          (else (list base '** exponent)))))

(define (base e)
  (let ((until** (until '** e)))
    (if (null? (cdr until**)) (car until**) until**)))
(define (exponent e)
  (let ((beyond** (beyond '** e)))
    (if (null? (cdr beyond**)) (car beyond**) beyond**)))

(define (eval-sum s)
  (if (sum? s) (make-sum (addend s) (augend s)) s))
(define (eval-product p)
  (if (product? p) (make-product (multiplier p) (multiplicand p)) p))
(define (eval-exponentiation e)
  (if (exponentiation? e) (make-exponentiation (base e) (exponent e)) e))
(define (eval exp)
  (eval-sum (eval-product (eval-exponentiation exp))))

;; using either of these algebraic rulesets, we can implement DERIV.
;; each cond clause has the form "if expression has this form, apply this rule"

(define (deriv exp var)
  (cond ((number? exp) 0) ;; deriv of const = 0
        ((variable? exp) 
         (if (same-variable? exp var) 1 0)) ;; d/dx(x) = 1, d/dx(y) = 0
        ((sum? exp)
         (make-sum (deriv (addend exp) var) ;; d/dx[a(x) + b(x)]
                   (deriv (augend exp) var))) ;; = d/dx[a(x)] + d/dx[b(x)]
        ((product? exp)
         (make-sum ;; d/dx[a(x)b(x)] = a(x)*d/dx[b(x)] + b(x)*d/dx[a(x)]
          (make-product (multiplier exp) 
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; some test cases of DERIV with Scheme (aka Polish) notation

#|

(deriv '(+ x 3) 'x) ; -> 1
(deriv '(* x y) 'x) ; -> y
(deriv '(* (* x y) (+ x 3)) 'x) ; -> (+ (* x y) (* y (+ x 3)))
(deriv '(*  x y (+ x 3)) 'x) ; -> (+ (* x y) (* y (+ x 3)))
(deriv '(* 3 (** x 2)) 'x) ; -> (* 3 (* 2 x))

|#

;; some test-cases of DERIV with standard notation

(deriv '(x + 3) 'x) ; -> 1
(deriv '(x * y) 'x) ; -> y
(deriv '(x * y * (x + 3)) 'x) ; -> ((x * y) + (y * (x + 3)))
(deriv '(x * y * x + 3) 'x) ; -> ((x * y) + (y * x))
(deriv '(3 * x ** 2) 'x) ; -> (3 * (2 * x))