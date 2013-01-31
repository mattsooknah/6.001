;;;;;;;;;;;;;;;; MUTATION ;;;;;;;;;;;;;;;;

(define last-cons
  (lambda (l)
    (if (null? (cdr l)) l
        (last-cons (cdr l)))))

;; Hidden inside the abstraction
(define (front-ptr q) (cadr q))
(define (rear-ptr q)  (cddr q))
(define (set-front-ptr! q item)
  (set-car! (cdr q) item))
(define (set-rear-ptr! q item)
  (set-cdr! (cdr q) item))

;; Implementation of the abstraction (see above)
(define (make-queue)
  (cons 'queue (cons nil nil)))

(define (queue? q)
  (and (pair? q) (eq? 'queue (car q))))

(define (empty-queue? q)
  (if (not (queue? q))                 ;defensive 
      (error "object not a queue:" q)  ;programming
      (null? (front-ptr q))))

(define (front-queue q)
  (if (empty-queue? q)
      (error "front of empty queue:" q)
      (car (front-ptr q))))

(define (insert-queue! q elt)
  (let ((new-pair (cons elt nil)))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "delete of empty queue:" q))
        (else
         (set-front-ptr! q 
            (cdr (front-ptr q)))
         q)))

(define fill-queue!
  (lambda (queue elts)
    (if (null? elts) queue
        (fill-queue! (insert-queue! queue (car elts)) (cdr elts)))))

;;;;;;;;;;;;;;;; ENVIRONMENTS ;;;;;;;;;;;;;;;;

(define (my-cons a b)
  (lambda (msg)
    (cond ((eq? msg 'car) a)
	  ((eq? msg 'cdr) b)
	  ((eq? msg 'set-car!)
	   (lambda (new) (set! a new)))
	  ((eq? msg 'set-cdr!)
	   (lambda (new) (set! b new))))))

(define (my-car p)
  (p 'car))

(define (my-cdr p)
  (p 'cdr))

(define (my-set-car! p new)
  ((p 'set-car!) new))

(define (my-set-cdr! p new)
  ((p 'set-cdr!) new))

(define make-clock
  (lambda ()
    (define sound 'silent) ;; initialize with some sound; it will get mutated later
    (lambda ()
      (if (eq? sound 'tick) 
          (set! sound 'tock) 
          (set! sound 'tick))
      sound
      )))

(define make-delay
  (lambda (init)
    (define later init) ;; load the init value into later
    (lambda (new)
      (define now later) ;; load later into now
      (set! later new) ;; change later to new 
      now))) ;; return now

(define make-long-delay
  (lambda (init-list)
    (define q (fill-queue! (make-queue) init-list)) ;; populates a queue with init-list
    (lambda (new)
      (define now (front-queue q)) ;; load the front element (the one to be returned) into now
      (delete-queue! q) ;; remove the front
      (insert-queue! q new) ;; stick the argument into the back
      now)))