;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Table implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-table void -> table
; table-get table, symbol -> (binding | null)
; table-put! table, symbol, anytype -> undef
; binding-value binding -> anytype

;;; Table implementation using alists
;;; A table is (table data), where data is a list of bindings
;;; A binding is a two-element list (symbol anytype)

(define table-tag 'table)

;;; table -> data
(define get-table-data cadr)

;;; table, data -> undef
(define set-table-data!
  (lambda (table new-data)
    (set-car! (cdr table) new-data)))

;;; void -> table
(define make-table 
  (lambda () (list table-tag '())))

;;; table, symbol -> (binding | null)
(define table-get 
  (lambda (table name)
    (assq name (get-table-data table))))

;;; table, symbol, anytype -> undef
(define table-put! 
  (lambda (table name value)
    (set-table-data! table (cons (list name value) 
				 (get-table-data table)))))

;;; binding -> anytype
(define binding-value cadr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Eval
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (tag-check e sym) 
  (and (pair? e) (eq? (car e) sym)))

(define (sum? e) 
  (tag-check e 'plus*))

(define (eval-sum exp) 
  (+ (eval (cadr exp)) (eval (caddr exp))))

(define (define? exp) 
  (tag-check exp 'define*))


(define environment (make-table))
(define (lookup name) 
  (let ((binding (table-get environment name)))
       (if (null? binding)
	   (error "unbound variable: " name)
	   (binding-value binding))))

(define (eval-define exp)
  (let ((name (cadr exp))
	(defined-to-be (caddr exp)))
    (table-put! environment name (eval defined-to-be))
    'undefined))


(define (greater? exp) (tag-check exp 'greater*))
(define (less? exp) (tag-check exp 'less*))
(define (if? exp) (tag-check exp 'if*))
(define (cond? exp) (tag-check exp 'cond*))
(define (and? exp) (tag-check exp 'and*))
(define (or? exp) (tag-check exp 'or*))

(define (eval exp) 
  (cond 
   ((number? exp) exp) 
   ((sum? exp) (eval-sum exp))
   ((symbol? exp) (lookup exp))
   ((define? exp) (eval-define exp))
   ((greater? exp) (eval-greater exp))
   ((less? exp) (eval-less exp))
   ((if? exp) (eval-if exp))
   ((and? exp) (eval-and exp))
   ((or? exp) (eval-or exp))
   ((cond? exp) (eval-cond exp))
   (else
    (error "unknown expression " exp))))

(define (eval-greater exp)
  (> (eval (cadr exp)) (eval (caddr exp))))

(define (eval-less exp)
  (< (eval (cadr exp)) (eval (caddr exp))))

(define (eval-if exp)
  (let ((predicate (cadr exp))
	(consequent (caddr exp))
	(alternative (cadddr exp)))
    (let ((test (eval predicate)))
      (cond
       ((eq? test #t) (eval consequent))
       ((eq? test #f) (eval alternative))
       (else (error "predicate not a conditional: "
		    predicate))))))

(define (eval-and exp)
  (let ((all-statements (cdr exp)))
    (define (and-iter statements)
      (if (null? statements) #t
          (let ((test (eval (car statements))))
            (cond
              ((eq? test #f) #f)
              ((eq? test #t) (and-iter (cdr statements)))
              (else 'error)))))
    (and-iter all-statements)))

(define (eval-or exp)
  (let ((all-statements (cdr exp)))
    (define (or-iter statements)
      (if (null? statements) #f
          (let ((test (eval (car statements))))
            (cond
              ((eq? test #t) #t)
              ((eq? test #f) (or-iter (cdr statements)))
              (else 'error)))))
    (or-iter all-statements)))

(define (eval-cond exp)
  (let ((condlist (cdr exp)))
    (define (cond-iter clauses)
      (if (null? clauses) #f
          (let ((predicate (caar clauses))
                (consequent (cadar clauses))
                (alternatives (cdr clauses)))
            (let ((test (eval predicate)))
              (cond 
                ((eq? test #t) (eval consequent))
                ((eq? test #f) (cond-iter alternatives))
                (else 'error))))))
    (cond-iter condlist)))
