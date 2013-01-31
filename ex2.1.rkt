#lang scheme

(define (average x y) (/ (+ x y) 2))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (let ((start-x (x-point (start-segment seg)))
        (start-y (y-point (start-segment seg)))
        (end-x (x-point (end-segment seg)))
        (end-y (y-point (end-segment seg))))
        (let ((mid-x (average start-x end-x))
              (mid-y (average start-y end-y)))
  (make-point mid-x mid-y))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rect p1 p2) (cons p1 p2))
(define (start-rect rect) (car rect))
(define (end-rect rect) (cdr rect))

(define (rect-width rect)
  (let ((start-x (x-point (start-rect rect)))
        (end-x (x-point (end-rect rect))))
  (abs (- start-x end-x))))

(define (rect-height rect)
  (let ((start-y (y-point (start-rect rect)))
        (end-y (y-point (end-rect rect))))
  (abs (- start-y end-y))))

(define (rect-area rect) (* (rect-height rect) (rect-width rect)))
(define (rect-perim rect) (* 2 (+ (rect-height rect) (rect-width rect))))
        
;; procedural representation of data (e.g. pairs)

(define (clons x y)
  (lambda (m) (m x y)))

(define (clar z)
  (z (lambda (p q) p)))

(define (cldr z)
  (z (lambda (p q) q)))

;; alternatively...

(define (power a b)
  (if (= b 0 ) 1
      (* a (power a (- b 1)))))

(define (thingy z a)
  (if (zero? (remainder z a)) (thingy (/ z a) a) z))

(define (log z a)
  (if (= z 1) 0
      (+ 1 (log (/ z a) a))))

(define (chons x y) (* (power 2 x) (power 3 y)))
(define (char z) (log (thingy z 3) 2))
(define (chdr z) (log (thingy z 2) 3))