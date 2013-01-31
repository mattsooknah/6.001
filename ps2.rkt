#lang racket

(define fast-exp
  (lambda (a b)
    (if (= b 1) a
    (if (even? b)
        (fast-exp (* a a) (/ b 2))
        (* a (fast-exp a (- b 1)))))))

(define simple-log-helper
  (lambda (n i)
    (if (= n 1) 
        i
        (simple-log-helper (/ n 2) (+ i 1)))))

(define simple-log
  (lambda (n)
    (simple-log-helper n 0)))

(define slow-mul 
  (lambda (a b) 
    (if (= b 0) 0
        (+ a (slow-mul a (- b 1))))))

(define inc (lambda (x) (+ x 1)))
(define dec (lambda (x) (- x 1)))

(define plus 
  (lambda (a b)
    (if (zero? b) a
        (inc (plus a (dec b))))))

(define (odd? x)
  (cond ((zero? x) #f)
        ((odd? (dec x)) #f)
        (else #t)))

(define boolean-odd?
  (lambda (x)
    (not (or (zero? x) (boolean-odd? (dec x))))))

(define slow-mul-iter
  (lambda (a b)
    (smi-helper a b 1)))

(define smi-helper
  (lambda (a b i)
    (if (> i b) 0
        (+ a (smi-helper a b (+ i 1))))))

(define double (lambda (x) (* x 2)))
(define halve (lambda (x) (/ x 2)))

(define fast-mul
  (lambda (a b)
    (cond ((= b 1) a)
          ((even? b) (fast-mul (double a) (halve b)))
          (else (+ a (fast-mul a (- b 1)))))))

(define quick-sum
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

(define fast-expi
  (lambda (a b)
    (fei-helper a b 1)))

(define fei-helper
  (lambda (a count prod)
    (if (zero? count) prod
        (if (even? count) (fei-helper (* a a) (halve count) prod)
            (fei-helper a (dec count) (* prod a))))))

(define sum-by-halves
  (lambda (a b)
    (if (= a b) a
        (+ (sum-by-halves a (floor (/ (+ a b) 2)))
           (sum-by-halves (+ (floor (/ (+ a b) 2)) 1) b)))))

(define clarity
  (lambda (a b)
    (if (> a b) 0
        (+ a (clarity (inc a) b)))))