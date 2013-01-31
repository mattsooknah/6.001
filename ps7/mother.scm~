;;; MOTHER.SCM
;;;

;; named-object
;;
(define (create-named-object name)
  (create-instance make-named-object name))

(define (make-named-object self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'named-object root-part)))
        ((NAME) (lambda () name))
        (else (get-method message root-part))))))

(define (names-of objects)
  ; Given a list of objects, returns a list of their names.
  (map (lambda (x) (ask x 'NAME)) objects))


;; Person
;;
(define (create-person name)
  (create-instance make-person name))

(define (make-person self name)
  (let ((named-part (make-named-object self name))
        (mother nil)
        (father nil)
        (children nil))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'person named-part)))
        ((SAY) (lambda (stuff) (display stuff)))
        ((MOTHER) (lambda () mother))
        ((FATHER) (lambda () father))
        ((CHILDREN) (lambda () children))
        ((SET-MOTHER!) (lambda (mom) (set! mother mom)))
        ((SET-FATHER!) (lambda (dad) (set! father dad)))
        ((ADD-CHILD) 
         (lambda (child)
           (set! children (cons child children))
           child))
        (else (get-method message named-part))))))


;; mother
;;
(define (create-mother name)
  (create-instance make-mother name))

(define (make-mother self name)
  (let ((person-part (make-person self name)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'mother person-part)))
        ((HAVE-CHILD)
         (lambda (dad child-name)
           (let ((child (create-person child-name)))
             (ask child 'set-mother! self)
             (ask child 'set-father! dad)
             (ask self 'add-child child)
             (ask dad 'add-child child))))
        (else (get-method message person-part))))))


;Tests
;(define a (create-mother 'anne))
;(define b (create-person 'bob))
;(ask a 'name)                           
;(ask b 'name)                           
;(ask a 'type)                           
;(ask b 'type)                           
;(define c (ask a 'have-child b 'cindy))
;(define d (ask a 'have-child b 'dan))  
;(names-of (ask a 'children))            
;(names-of (ask b 'children))            
;(ask d 'name)                           
;(ask (ask d 'mother) 'name)             
;(ask (ask d 'father) 'name)             
;(show-instance d)