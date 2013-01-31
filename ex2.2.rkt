#lang racket

(define (reverse list)
  (if (null? list) empty
      (append (reverse (cdr list)) (cons (car list) empty))))

(define (deep-reverse list)
  (cond ((null? list) empty)
        ((not (pair? list)) list)
        (else (append (deep-reverse (cdr list)) (cons (deep-reverse (car list)) empty)))))

(define (fringe list)
  (cond ((null? list) empty)
        ((not (pair? list)) (cons list empty))
        (else (append (fringe (car list)) (fringe (cdr list))))))

(define (make-mobile left right) (list left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (make-branch length structure) (list length structure))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (branch-torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      #t))
      
(define (balanced? mobile)
  (and (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))
       (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))))

;(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
;(define b (make-mobile (make-branch 2 3) (make-branch 4 5)))
;(total-weight a) ;-> 6
;(total-weight b) ;-> 8
;(balanced? a) ;-> #t
;(balanced? b) ;-> #f

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) empty)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-branch-map branch)
  (if (pair? branch) 
      (square-tree-map branch)
      (square branch)))

(define (square-tree-map tree)
  (map square-branch-map tree))

(define (tree-map proc tree)
  (cond ((null? tree) empty)
        ((pair? tree) 
         (cons (tree-map proc (car tree)) 
               (tree-map proc (cdr tree))))
        (else (proc tree))))

(define (branch-map-clever proc)
  (lambda (branch)
    (if (pair? branch)
        (tree-map-clever proc branch)
        (proc branch))))

(define (tree-map-clever proc tree)
  (map (branch-map-clever proc) tree))

(define (append-to list1)
  (lambda (list2) (append list1 list2)))

(define (subsets s)
  (if (null? s) (list empty)
      (let ((first (list (car s)))
            (rest (subsets (cdr s))))
        (append rest (map (append-to first) rest)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map proc sequence)
  (accumulate 
   (lambda (x y) (cons (proc x) y)) 
   empty sequence))

(define (append s1 s2)
  (accumulate cons s2 s1))

(define (length sequence)
  (accumulate
   (lambda (x y) (+ 1 y))
   0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (flatten-branch branch)
  (if (pair? branch) 
      (flatten branch)
      (list branch)))

(define (flatten tree)
  (accumulate append 
              empty 
              (map flatten-branch tree)))

(define (count-leaves tree) (length (flatten tree)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons empty m))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (x) matrix-*-vector n-cols x) m)))

(define (flatmap proc seq)
  (accumulate append empty (map proc seq)))

(define (permutations s)
  (if (null? s)                      ; empty set?
      (list empty)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (enumerate-interval a b)
  (if (> a b) empty (cons a (enumerate-interval (+ a 1) b))))

(define (pair-with i)
  (lambda (j) (list i j)))
  
(define (generate-ordered-pairs i)
  (map (pair-with i) (enumerate-interval 1 (- i 1))))

(define (unique-pairs n)
  (flatmap generate-ordered-pairs (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (divisible-by n)
  (lambda (x) (= 0 (remainder x n))))

(define (prime? x)
  (define (prime-helper x n)
    (cond ((>= n x) #t)
          (((divisible-by n) x) #f)
          (else (prime-helper x (+ n 1)))))
  (prime-helper x 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(define (generate-ordered-triples i)
  (map (lambda (j) (cons i j)) (unique-pairs (- i 1))))

(define (unique-triples n)
  (flatmap generate-ordered-triples (enumerate-interval 1 n)))

(define (sum list)
  (accumulate + 0 list))

(define (fixed-sum-triples n s)
  (filter (lambda (x) (= s (sum x))) (unique-triples n)))