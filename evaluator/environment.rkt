;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; THE ENVIRONMENT MODEL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Procedures ("double bubbles")
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))
(define (procedure-parameters proc) (second proc))
(define (procedure-body proc) (third proc))
(define (procedure-environment proc) (fourth proc))

;; Binding creation and alteration
(define (make-binding var val)
  (list var val val))
(define binding-variable car)
(define current-binding-value cadr)
(define original-binding-value caddr)
(define (binding-search var frame)
  (if (null? frame)
      #f
      (if (eq? var (first (first frame)))
          (first frame)
          (binding-search var (rest frame)))))       
(define (set-binding-value! binding val)
  (set-car! (cdr binding) val))
(define (define-binding-value! binding val)
  (set-car! (cdr binding) val)
  (set-car! (cddr binding) val))
(define (reset-binding-value! binding)
  (set-car! (cdr binding) (caddr binding)))

;; Frames
(define (make-frame creator-proc variables values)
  (append (list 'frame creator-proc) (map make-binding variables values)))
(define (frame-creator frame) (cadr frame))
(define (frame-variables frame) (map binding-variable (cddr frame)))
(define (frame-values frame) (map binding-value (cddr frame)))
(define (add-binding-to-frame! var val frame)
  (set-cdr! (cdr frame) (cons (make-binding var val) (cddr frame))))
(define (find-in-frame var frame)
  (binding-search var (cddr frame)))
(define no-creator-procedure '())

;; Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (contains-proc-scope? proc env)
  (cond ((eq? env the-empty-environment) #f)
        ((eq? proc (frame-creator (first-frame env))) #t)
        (else (contains-proc-scope? proc (enclosing-environment env)))))

(define (find-in-environment var env)
  (if (eq? env the-empty-environment)
      #f
      (let* ((frame (first-frame env))
             (binding (find-in-frame var frame)))
        (if binding
            binding
            (find-in-environment var (enclosing-environment env))))))

(define (extend-environment creator-proc vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame creator-proc vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many args supplied" vars vals)
          (error "Too few args supplied" vars vals))))

;; Binding lookup
(define (lookup-variable-value var env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (current-binding-value binding)
        (error "Unbound variable -- LOOKUP" var))))

(define (set-variable-value! var val env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (set-binding-value! binding val)
        (error "Unbound variable -- SET" var))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (find-in-frame var frame)))
    (if binding
        (define-binding-value! binding val)
        (add-binding-to-frame! var val frame))))

(define (reset-variable-value! var env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (reset-binding-value! binding)
        (error "Unbound variable -- RESET" var))))

;; Advice

(define (make-advice type params body env)
  (list type params body env))
(define (advice-parameters advice) (second advice))
(define (advice-body advice) (third advice))
(define (advice-environment advice) (fourth advice))

(define (before-procedure? advice) (tagged-list? advice 'before-procedure))
(define (around-procedure? advice) (tagged-list? advice 'around-procedure))

(define (add-advice! proc-name advice constraint env)
  (let* ((old-proc (lookup-variable-value proc-name env))
         (new-proc (list 'advised proc-name advice old-proc constraint env)))
    (set-variable-value! proc-name new-proc env)))

(define (has-advice? proc) (tagged-list? proc 'advised))
(define (name-part advised-proc) (second advised-proc))
(define (advice-part advised-proc) (third advised-proc))
(define (procedure-part advised-proc) (first (cdddr advised-proc)))
(define (constraint-part advised-proc) (second (cdddr advised-proc)))
(define (environment-part advised-proc) (third (cdddr advised-proc)))

(define (in-constraint? constraint) (tagged-list? constraint 'in))
(define (scoping-proc-name in-constraint) (second in-constraint))

(define (constraint-satisfied? advised-proc env)
  (let ((constraint (constraint-part advised-proc)))
    (or (null? constraint)
        (cond ((in-constraint? constraint)
               (let ((scoping-proc 
                      (lookup-variable-value (scoping-proc-name constraint) (environment-part advised-proc))))
                 (contains-proc-scope? scoping-proc env)))
              (else (error "Unknown constraint" constraint))))))
