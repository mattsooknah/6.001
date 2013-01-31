;;; code for city navigation problem set

;;; constructors for data abstractions

(define make-segment          ; string, point, point -> segment
  (lambda (name start finish) (list name start finish)))

(define make-point            ; number,number -> Point
  (lambda (x y)             (list x y)))

(define make-map list)       ; segment, ... -> list<segment>

(define make-trip list)      ; segment, ... -> list<segment>


;;; selectors for data abstractions

(define segment-name
  (lambda (segment) (car segment)))

(define segment-start-point
  (lambda (segment) (cadr segment)))

(define segment-end-point
  (lambda (segment) (caddr segment)))

(define x-coor
  (lambda (point) (car point)))

(define y-coor
  (lambda (point) (cadr point)))

(define segment-start-x
  (lambda (seg)
    (x-coor (segment-start-point seg))))

(define segment-end-x
  (lambda (seg)
    (x-coor (segment-end-point seg))))

(define segment-start-y
  (lambda (seg)
    (y-coor (segment-start-point seg))))

(define segment-end-y
  (lambda (seg)
    (y-coor (segment-end-point seg))))

;;; match data abstraction

(define make-match list)                 ; X, Y -> match<X,Y>
(define first-of-match car)              ; match<X,Y> -> X
(define second-of-match cadr)            ; match<X,Y> -> Y

;;; implementation of correspondence finder for lists of segments

; remove-elt: given an element E and a list L in which E occurs at
; most once, return a list containing all elements in L except E. If E
; does not occur in L, return a list containing all elements in L.
; Elements should be compared using the function equal? in order to
; support lists of arbitrary element types.

(define remove-elt
  (lambda (elt list)
    (cond ((null? list) nil)
          ((eq? elt (car list)) (cdr list))
          (else (cons (car list) (remove-elt elt (cdr list)))))))

; map-and-flatten: this function can be useful when mapping a function
; whose type is X -> list<Y> over some list.  If you use regular map,
; the return type will be list<list<Y>>.  This function appends the
; resulting sublists, returning list<Y>.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define map-and-flatten
  (lambda (func list)
    (accumulate append '() (map func list))))

; all-correspondences: given lists l1 and l2, length(l1) <= length(l2),
; return all correspondences.  A correspondence is a list of matches.
; Each match joins an element from l1 with an element from l2. Within a
; given correspondence, no elements from l1 are duplicated and no
; elements from l2 are duplicated. All elements from l1 appear in
; each correspondence, while some elements from l2 may not appear (if 
; l2 is longer than l1).
;
; type:  list(X), list(Y) -> list(list(match(X,Y)))

(define all-correspondences
  (lambda (l1 l2)
    (if (null? l1)
        (list nil)
       (map-and-flatten
          (lambda (choice-from-l2)
            (let ((initial-match (make-match (car l1) choice-from-l2))
                  (correspondences-of-remaining-elts
                    (all-correspondences (cdr l1) 
                                         (remove-elt choice-from-l2 l2))))
              (map 
               (lambda (correspondence-of-rest) 
                 (cons initial-match correspondence-of-rest))
               correspondences-of-remaining-elts)
            ))
          l2))))

;; abstraction for segments with unknown names (essentially wildcards)

(define make-unnamed-segment      ; point, point -> segment
  (lambda (start finish) (list internal-name-for-unnamed-segment start finish)))

(define unnamed-segment?          ; segment -> boolean
  (lambda (segment) (eq? (car segment) internal-name-for-unnamed-segment)))

(define internal-name-for-unnamed-segment 'foo)

(define (same-name? match)
  (let ((seg1 (first-of-match match))
        (seg2 (second-of-match match)))
    (or (unnamed-segment? seg1)
        (unnamed-segment? seg2)
        (string=? (segment-name seg1)
                  (segment-name seg2)))))

(define filtered-correspondences
  (lambda (pred? l1 l2)
    (if (null? l1)
        (list nil)
       (map-and-flatten
          (lambda (choice-from-l2)
            (let ((initial-match (make-match (car l1) choice-from-l2))
                  (correspondences-of-remaining-elts
                   (filtered-correspondences pred? (cdr l1) (remove-elt choice-from-l2 l2))))
              (if (pred? initial-match)
                  (map 
                   (lambda (correspondence-of-rest)
                     (cons initial-match correspondence-of-rest))
                   correspondences-of-remaining-elts)
                  nil)
              ))
          l2)))) 

;;; some trial data

(define mapseg-1 (make-segment "beacon"  (make-point 0 6) (make-point 3 7)))
(define mapseg-2 (make-segment "beacon"  (make-point 3 7) (make-point 6 8)))
(define mapseg-3 (make-segment "beacon"  (make-point 6 8) (make-point 9 9)))
(define mapseg-4 (make-segment "beacon"  (make-point 9 9) (make-point 12 10)))

(define mapseg-5 (make-segment "massachusetts"  (make-point 0 6) (make-point 1 4)))
(define mapseg-6 (make-segment "massachusetts"  (make-point 1 4) (make-point 2 2)))

(define mapseg-7 (make-segment "masspike"  (make-point 1 4) (make-point 5 3)))
(define mapseg-8 (make-segment "masspike"  (make-point 5 3) (make-point 9 2)))

(define mapseg-9 (make-segment "essex"  (make-point 2 2) (make-point 5 3)))
(define mapseg-10 (make-segment "essex"  (make-point 5 3) (make-point 8 4)))
(define mapseg-11 (make-segment "essex"  (make-point 8 4) (make-point 11 5)))
(define mapseg-12 (make-segment "essex"  (make-point 11 5) (make-point 12 6)))

(define mapseg-13 (make-segment "clarendon" (make-point 9 2) (make-point 8 4)))
(define mapseg-14 (make-segment "clarendon" (make-point 8 4) (make-point 7 7)))
(define mapseg-15 (make-segment "clarendon" (make-point 7 7) (make-point 6 8)))

(define mapseg-16 (make-segment "arlington" (make-point 9 9) (make-point 11 5)))
(define mapseg-17 (make-segment "arlington"  (make-point 11 5) (make-point 12 3)))

(define mapseg-18 (make-segment "park"  (make-point 12 12) (make-point 12 10)))
(define mapseg-19 (make-segment "park"  (make-point 12 10) (make-point 13 9)))

(define mapseg-20 (make-segment "cambridge"  (make-point 12 12) (make-point 13 12)))

(define mapseg-21 (make-segment "tremont"  (make-point 13 12) (make-point 14 11)))
(define mapseg-22 (make-segment "tremont"  (make-point 14 11) (make-point 14 10)))
(define mapseg-23 (make-segment "tremont" (make-point 14 10) (make-point 13 9)))

(define mapseg-24 (make-segment "mason"  (make-point 12 6) (make-point 12 7)))
(define mapseg-25 (make-segment "mason"  (make-point 12 7) (make-point 13 9)))

(define trial-map-1
    (make-map mapseg-1 mapseg-2 mapseg-3 mapseg-4 mapseg-5 mapseg-6
              mapseg-7 mapseg-8 mapseg-9 mapseg-10 mapseg-11 mapseg-12
              mapseg-13 mapseg-14 mapseg-15 mapseg-16 mapseg-17 mapseg-18
              mapseg-19 mapseg-20 mapseg-21 mapseg-22 mapseg-23 mapseg-24
              mapseg-24 mapseg-25))

(define trial-trip-1 
    (make-trip (make-segment "essex" (make-point 0 0) (make-point 1 1))))

(define trial-trip-2
    (make-trip 
               (make-unnamed-segment (make-point 0 0) (make-point 4 2))
               (make-segment "essex" (make-point 4 2) (make-point 5 -1))
               ))

(define trial-trip-3
    (make-trip 
               (make-unnamed-segment (make-point 0 3) (make-point 1 3))
               (make-unnamed-segment (make-point 1 3) (make-point 3 2))
               (make-segment "tremont" (make-point 3 2) (make-point 4 1))))