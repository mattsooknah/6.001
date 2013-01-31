(load "mother.scm")

(define a (create-mother 'anne))
(define b (create-person 'bob))
(define c (ask a 'have-child b 'cindy))
(define d (ask a 'have-child b 'dan))

#|
(names-of (ask c 'siblings))
;Value: (dan)

(names-of (ask d 'siblings))
;Value: (cindy)

(names-of (ask a 'siblings))
;Value: #f
|#