;;;
;;; Bank Account Abstractions
;;;

(load "objsys.scm")

;;  Named-Object Class
;;
(define (create-named-object name)      ; symbol -> named-object
  (create-instance make-named-object name))

(define (make-named-object self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'named-object root-part)))
        ((NAME) (lambda () name))
        (else (get-method message root-part))))))
  
;;  Account Class
;;
(define (create-account name balance)
  (create-instance make-account name balance))

(define (make-account self name balance)
  (let ((named-part (make-named-object self name)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'account named-part)))
	((BALANCE) (lambda () balance))
	((DEPOSIT) (lambda (amount)
		     (set! balance (+ balance amount))
		     (ask self 'balance)))
	((WITHDRAW) (lambda (amount)
		      (cond ((> amount balance)
			     (set! balance 0)
			     (ask self 'balance))
			    (else (set! balance (- balance amount))
				  (ask self 'balance)))))
	(else (get-method message named-part))))))

;;  Savings Class
;;
(define (create-savings name init)
  (create-instance make-savings name init))

(define (make-savings self name init)
  (let ((account (make-account self name init)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'savings account)))
	((WITHDRAW) (lambda (amount)
		      (if (> amount (- (ask account 'balance) init))
			  'not-enough-funds
			  (ask account 'withdraw amount))))
	(else (get-method message account))))))

;; Checking Class
;;
(define (create-checking name init savings)
  (create-instance make-checking name init savings))

(define (make-checking self name init savings)
  (let ((account (make-account self name init)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'checking account)))
    ((WITHDRAW) (lambda (amount)
                  (let* ((old-balance (ask account 'balance))
                         (new-balance (- old-balance amount)))
                    (if (< new-balance 0)
                        (begin (ask savings 'withdraw (* -1 new-balance))
                               (ask account 'withdraw old-balance))
                        (ask account 'withdraw amount)))))
    ((TRANSFER) (lambda (amount)
                  (let ((old-balance (ask account 'balance)))
                    (if (> amount old-balance) (set! amount old-balance))
                    (ask account 'withdraw amount)
                    (ask savings 'deposit amount))))
        (else (get-method message account))))))

