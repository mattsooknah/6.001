(define (make-accumulator sum)
  (lambda (new)
    (if (number? new)
        (begin (set! sum (+ sum new))
               sum)
        (error "NaN: " new))))

(define (make-monitored f)
  (define count 0)
  (define (how-many-calls?) count)
  (define (reset-count) (set! count 0))
  (define (mf m)
    (cond ((eq? m 'how-many-calls?) (how-many-calls?))
          ((eq? m 'reset-count) (reset-count))
          (else (begin (set! count (+ count 1)) (f m)))))
  mf)

(define (contains? list elt)
  (cond ((null? list) #f)
        ((eq? elt (car list)) #t)
        (else (contains? (cdr list) elt))))

(define (make-account balance password)
  (define pw-list (cons password nil))
  (define (add-pw new-pw)
    (set! pw-list (cons new-pw pw-list)))
  (define (valid-pw? input-pw)
    (contains? pw-list input-pw))
  (define failed-attempts 0)
  (define (pw-fail amount)
    (if (> failed-attempts 7)
        (call-the-cops)
        "Incorrect password"))
  (define (withdraw amount)
    (if (<= amount balance)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (access input-pw command)
    (if (valid-pw? input-pw)
        (begin (set! failed-attempts 0)
               (cond ((eq? command 'add-pw) add-pw)
                     ((eq? command 'withdraw) withdraw)
                     ((eq? command 'deposit) deposit)
                     (else (error "Unknown command: " command))))
        (begin (set! failed-attempts (+ failed-attempts 1))
               pw-fail)))
  access)

(define (make-joint account old-pw new-pw)
  ((account old-pw 'add-pw) new-pw)
  account)
#|
(define peter-acc (make-account 100 'foo))
((peter-acc 'bar 'withdraw) 100)
;"Incorrect password"
((peter-acc 'foo 'withdraw) 10)
;90
(define paul-acc (make-joint peter-acc 'foo 'bar))
((paul-acc 'foo 'withdraw) 10)
;80
((paul-acc 'bar 'withdraw) 10)
;70
((peter-acc 'bar 'withdraw) 10)
;60
|#
(define (call-the-cops)
  "Account locked. The authorities have been notified of your attempts to break in.")

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high precision)
  (let ((range (- high low))
        (order (expt 10 precision)))
    (+ low (/ (random (* order range)) order))))

(define (estimate-integral shape x1 x2 y1 y2 trials)
  (let ((area (* (- x2 x1) (- y2 y1))) 
        (p (monte-carlo trials (shape x1 x2 y1 y2))))
    (* p area 1.0)))

(define (unit-circle-test x1 x2 y1 y2)
  (lambda ()
    (let ((x (random-in-range x1 x2 2))
          (y (random-in-range y1 y2 2)))
      (<= (+ (* x x) (* y y)) 1))))

(define (rand-update x) (+ x (sqrt (abs x))))

(define (make-rand init)
  (define x init)
  (define (generate)
    (set! x (rand-update x)) x)
  (define (run command)
    (cond ((eq? command 'generate) (generate))
          ((eq? command 'reset) (lambda (new-init) (set! x new-init)))
          (else (error "Invalid command"))))
  run)

(define (thingy)
  (define value 0)
  (lambda (n)
    (define output value)
    (set! value n)
    output))

(define f (thingy))