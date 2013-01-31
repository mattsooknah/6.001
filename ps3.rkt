#lang racket

(define nil empty)

(define (listify lower upper)
  (if (> lower upper) nil
      (cons lower (listify (+ 1 lower) upper))))

(define (list-length list)
  (if (null? list) 0 (+ 1 (list-length (cdr list)))))

(define (map proc lst)
  (if (null? lst)
      empty
      (cons (proc (car lst))
            (map proc (cdr lst)))))

(define (reduce op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (reduce op init (cdr lst)))))

(define (filter pred lst)
  (cond ((null? lst) empty)
        ((pred (car lst))
         (cons (car lst)
               (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (lengths lst)
  (map string-length lst))

(define (total-length lst)
  (reduce + 0 (lengths lst)))

(define (string-em-up lst)
  (total-length (filter string? lst)))

(define (make-multiplier x)
  (lambda (y) (* x y)))

(define (contains? list elt)
  (cond ((null? list) #f)
        ((equal? elt (car list)) #t)
        (else (contains? (cdr list) elt))))

(define (remove-duplicates list)
  (cond ((null? list) empty)
        ((contains? (cdr list) (car list)) (remove-duplicates (cdr list)))
        (else (cons (car list) (remove-duplicates (cdr list))))))

(define (union list1 list2)
  (remove-duplicates (append list1 list2)))

(define (subset? list1 list2)
  (cond ((null? list1) #t)
        ((not (contains? list2 (car list1))) #f)
        (else (subset? (cdr list1) list2))))

(define (intersection list1 list2)
  (define (helper list1 list2)
    (cond ((null? list1) nil)
          ((contains? list2 (car list1))
           (cons (car list1) (intersection (cdr list1) list2)))
          (else (intersection (cdr list1) list2))))
  (remove-duplicates (helper list1 list2)))

(define (every-other list)
  (if (or (null? list) (null? (cdr list))) list
      (cons (car list) (every-other (cddr list)))))

(define (count-true-rec pred lower upper)
  (cond ((> lower upper) 0)
        ((pred lower) (+ 1 (count-true-rec pred (+ 1 lower) upper)))
        (else (count-true-rec pred (+ 1 lower) upper))))

(define (count-true-fancy pred lower upper)
  (list-length (filter pred (listify lower upper))))

(define (count-true-iter pred lower upper)
  (define (helper pred lower upper count)
    (cond ((> lower upper) count)
          ((pred lower) (helper pred (+ 1 lower) upper (+ 1 count)))
          (else (helper pred (+ 1 lower) upper count))))
  (helper pred lower upper 0))

(define (accumulate-interval op init lower upper)
  (if (> lower upper) init
      (op lower (accumulate-interval op init (+ 1 lower) upper))))

(define compose (lambda (f g) (lambda (x) (f (g x)))))

(define (inc x) (+ x 1))

(define (repeatedly-apply p n)
  (if (= n 1) p
      (compose p (repeatedly-apply p (- n 1)))))

(define (curry p)
  (lambda (x) (lambda (y) (p x y))))

(define (mix-it-up list)
  (define (helper x)
    (if (odd? x) (* x 2) (/ x 2)))
  (map helper list))

(define (accumulate-filtered-interval pred op init lower upper)
  (cond ((> lower upper) init)
        ((pred lower) 
         (op lower (accumulate-filtered-interval pred op init (+ 1 lower) upper)))
        (else (accumulate-filtered-interval pred op init (+ 1 lower) upper))))

(define (count-true pred lower upper)
  (define (y-counter x y) (+ 1 y))
  (accumulate-filtered-interval pred y-counter 0 lower upper))