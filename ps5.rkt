;;;;;;;;;;;;;;;; LECTURE 10 ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; PROBLEM 1 ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Constant expressions ;;;;;;;;;;;;;;;;

(define constant-tag 'constant)

;;; number -> ConstantExp
(define make-constant (lambda (num)
    (list constant-tag num)))
  
;;; number -> RangeExp
(define make-canonical-constant
  (lambda (num)
    (make-range num num)))

;;; anytype -> boolean
(define constant-exp? (lambda (exp)
  (and (pair? exp)
       (eq? (car exp) constant-tag))))

;;; ConstantExp -> number
(define constant-val cadr)

;;; ConstantExp,ConstantExp->ConstantExp
(define constant-add (lambda (c1 c2)
    (make-constant (+ (constant-val c1)
		      (constant-val c2)))))

;;;;;;;;;;;;;;;;  Range expressions ;;;;;;;;;;;;;;;;

(define range-tag 'range)

;;; number, number -> RangeExp
(define make-range (lambda (lower upper)
  (list range-tag lower upper)))

;;; anytype -> boolean
(define range-exp? (lambda (exp)
  (and (pair? exp)
       (eq? (car exp) range-tag))))

;;; RangeExp -> number
(define range-min cadr)
(define range-max caddr)

;;; RangeExp,RangeExp -> RangeExp
(define range-add (lambda (r1 r2)
  (make-range (+ (range-min r1) (range-min r2))
	      (+ (range-max r1) (range-max r2)))))

;;;;;;;;;;;;;;;; Limited Precision expressions ;;;;;;;;;;;;;;;;

(define limited-tag 'limited)

;;; anytype -> boolean
(define limited-exp? (lambda (exp)
  (and (pair? exp)
       (eq? (car exp) limited-tag))))

;;; number,number -> LimitedExp
(define make-limited-precision (lambda (val err)
  (list limited-tag val err)))

;;; number,number -> RangeExp
(define make-canonical-limited-precision 
  (lambda (val err)
    (make-range (- val err) (+ val err))))

;;;;;;;;;;;;;;;; Sum expression ;;;;;;;;;;;;;;;;

(define sum-tag '+)

;;; number,number -> SumExp
(define make-sum (lambda (addend augend)
  (list sum-tag addend augend)))

;;; anytype -> boolean
(define sum-exp? (lambda (exp)
  (and (pair? exp)
       (eq? (car exp) sum-tag))))

;;; SumExp -> number
(define sum-addend cadr)
(define sum-augend caddr)

;;;;;;;;;;;;;;;; Dealing with values in general ;;;;;;;;;;;;;;;;
;;;
;;; ValueExp = ConstantExp | RangeExp

;;; anytype -> boolean
(define value-exp? (lambda (v)
  (or (constant-exp? v) (range-exp? v))))

;;; ValueExp, ValueExp -> ValueExp
(define value-add (lambda (v1 v2)
  (cond ((and (constant-exp? v1) (constant-exp? v2))
	 (constant-add v1 v2))
	((and (value-exp? v1) (value-exp? v2))
	 (range-add (val2range v1) (val2range v2)))
	(else
	 (error "Unknown exp type")))))

;;; ValueExp -> RangeExp
(define val2range (lambda (v)
  (if (range-exp? v)
      v		    
      (make-range (constant-val v) (constant-val v)))))

;;;;;;;;;;;;;;;; Eval ;;;;;;;;;;;;;;;;

;;; ValueExp | LimitedExp | SumExp -> ValueExp | LimitedExp

(define eval-exp (lambda (exp)
  (cond ((value-exp? exp) exp)
	((limited-exp? exp) exp)
	((sum-exp? exp)
	 (value-add (eval-exp (sum-addend exp)) (eval-exp (sum-augend exp))))
	(else (error "Unknown expr type")))))

;;;;;;;;;;;;;;;; Output ;;;;;;;;;;;;;;;;

;;; RangeExp -> ValueExp | LimitedExp
(define output-value
  (lambda (exp)
    (if (range-exp? exp)
        (cond ((= (range-min exp) (range-max exp)) (list constant-tag (range-min exp))) 
              (*output-ranges-as-limited-precision*
               (list limited-tag 
                     (/ (+ (range-min exp) (range-max exp)) 2) 
                     (/ (- (range-max exp) (range-min exp)) 2)))
              (else exp))
        'error)))
        
(define *output-ranges-as-limited-precision* #t)

;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;;;

(define (make-rat num den)
  (list 'rat num den))

(define (tagged-list? elt tag)
  (if (pair? elt)
      (eq? (car elt) tag) #f))

(define (rat? elt)
  (tagged-list? elt 'rat))

(define (num rat)
  (if (rat? rat)
      (cadr rat)
      (error rat " not a rational number")))

(define (denom rat)
  (if (rat? rat)
      (caddr rat)
      (error rat " not a rational number")))

(define (+rat r1 r2)
  (if (and (rat? r1) (rat? r2))
      (let ((a (num r1))
            (b (denom r1))
            (c (num r2))
            (d (denom r2)))
        (make-rat (+ (* a d) (* b c)) (* b d)))
      (error "arguments not both rational")))

(define (+rat1 r1 r2)
  (if (and (rat? r1) (rat? r2))
      (let ((a (num r1))
            (b (denom r1))
            (c (num r2))
            (d (denom r2)))
        (let ((n (+ (* a d) (* b c)))
              (d (* b d)))
          (make-rat (/ n (gcd n d)) (/ d (gcd n d)))))
      (error "arguments not both rational")))

(define (make-integer num)
  (list 'integer num))

(define (integer? elt)
  (tagged-list? elt 'integer))

(define (number elt)
  (if (integer? elt)
      (cadr elt)
      (error elt " not an integer")))

(define (+int i1 i2)
  (if (and (integer? i1) (integer? i2))
      (make-integer (+ (number i1) (number i2)))
      (error "arguments not both integers")))

(define (convert-to-rat elt)
  (cond ((rat? elt) elt)
        ((integer? elt) (make-rat (number elt) 1))
        (else (error elt " not an integer or rational"))))

(define (add n1 n2)
  (if (and (integer? n1) (integer? n2))
      (+int n1 n2)
      (+rat1 (convert-to-rat n1) (convert-to-rat n2))))
         
;;;;;;;;;;;;;;;; LECTURE 11 ;;;;;;;;;;;;;;;;

(define find-assoc-2
  (lambda (key alist)
    (cond ((null? alist) #f)
          ((eq? key (caar alist)) (car alist))
          (else (find-assoc-2 key (cdr alist))))))

(define lookup
  (lambda (key alist)
    (let ((assoc (find-assoc-2 key alist)))
      (if (pair? assoc) 
          (list #t (cadr assoc))
          (list #f #f)))))

;;;;;;;;;;;;;;;; PROBLEM 1 ;;;;;;;;;;;;;;;;

(define make-color
  (lambda (red-amt green-amt blue-amt)
    (list (restrict red-amt)
	  (restrict green-amt)
	  (restrict blue-amt))))

(define red-val car)
(define green-val cadr)
(define blue-val caddr)

(define restrict
  (lambda (x)
    (cond ((< x 0) 0)
	  ((> x 255) 255)
	  (else (round x)))))

(define half-as-bright
  (lambda (color)
    (map (lambda (val) (/ val 2)) color)))

;;;;;;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;;;;;;;

(define table3-tag 'table3)

;;; Make a table with entries from 0 to size-1.
;;; type: number,A -> table3<A>
(define make-table3 
  (lambda (size initial-value)
    (list table3-tag
	  size
	  (make-vector size initial-value))))

(define get-table3-size cadr)
(define get-table3-vector caddr)

;;; type: anytype->boolean
(define table3? 
  (lambda (thing)
    (and (pair? thing) (eq? (car thing) table3-tag))))

(define table3-key?
  (lambda (thing table)
    (if (table3? table)
        (and (integer? thing) (positive? thing) (< thing (get-table3-size table)))
        'error)))

(define table3-get
  (lambda (table key)
    (if (and (table3? table) (table3-key? key table))
        (vector-ref (get-table3-vector table) key)
        'error)))