#lang racket

(define (square x) (* x x))
(define (sos x y) (+ (square x) (square y)))
(define (f x) (sos (+ x 1) (* x 2)))

(define (max2sos x y z)
  (cond ((and (>= x z) (>= y z)) (sos x y))
        ((and (>= x y) (>= z y)) (sos x z))
        (else (sos y z))))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (/ (abs (- (improve guess) guess)) guess) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))