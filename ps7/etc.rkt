(load "bank.scm")
 
(define (create-counter)
  (create-instance make-counter))

(define (make-counter self)
  (let ((root (make-root-object self))
        (count 0)
        (times-reset 0))
    (lambda (msg)
      (case msg
        ((type)  (lambda () (type-extend 'counter root)))
        ((state) (lambda () count))
        ((count) (lambda () (begin (set! count (+ count 1)) count)))
        ((reset) (lambda () (begin (set! count 0) (set! times-reset (+ times-reset 1)) count)))
  ((times-reset) (lambda () times-reset))
	(else (get-method msg root))))))
 
