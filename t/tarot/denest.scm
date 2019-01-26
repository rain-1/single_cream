(define (make-let-form table body)
 (list 'let-form table body))
(define (let-form? l) (and (list? l) (= 3 (length l)) (eq? (list-ref l 0) 'let-form)))
(define (let-form-table l) (list-ref l 1))
(define (let-form-body l) (list-ref l 2))

;; the table is a sequence of let bindings

(define (let-form->let l)
  `(let ,(seq->list (let-form-table l))
     ,(let-form-body l)))

(define (simple? exp)
  (or (var? exp)
      (datum? exp)))

(define (simplify l)
  (if (simple? (let-form-body l))
      l
      (let ((t (gensym "tmp")))
	(make-let-form `(cat ,(let-form-table l) (elt (,t ,(let-form-body l))))
		       `(var tmp ,t)))))

;; The denest function comes in 3 different variations
;;
;; (denest exp simple)
;;   denests exp requiring the let body to be simple if simple = #t
;;   outputs a let-form record
;;
;; (denest-exp exp)
;;   denest exp without worrying about making the result simple
;;   outputs a let-form record
;;
;; (denest^ exp)
;;   this is a helper function
;;   denest exp, don't require let body to be simple
;;   converts the result to an s-expression
;;

(define (denest exp simple)
  ;; simple #t means the let body must be a variable/datum
  ;; simple #f means allow complex let bodies
  (let ((l (denest-exp exp)))
    (if simple (simplify l) l)))

(define (denest^ exp)
  (let-form->let (denest exp #f)))

(define (denest-exp exp)
  (cond ((datum? exp) (make-let-form `(cat) exp))

	((symbol? exp) (make-let-form `(cat) exp))

	((var? exp) (make-let-form `(cat) exp))

	((if? exp)
	 (let ((l (denest-exp (if-get-test exp))))
	   (make-let-form (let-form-table l)
			  `(if ,(let-form-body l)
			       ,(denest^ (if-get-consequent exp))
			       ,(denest^ (if-get-antecedent exp))))))

	;; ((lambda? exp)
	;;  (make-let-form `(cat) `(lambda ,(lambda-get-vars exp)
	;; 			  ,(denest^ (lambda-get-body exp)))))

	((closure? exp)
	 ;; TODO: simpler version if clo has no env
	 (let* ((clo (gensym "clo"))
		(binding (list clo exp)))
	   (make-let-form `(cat ,(denest-letrec-binding-make-closure binding)
				,(denest-letrec-binding-setup-env binding))
			  `(var tmp ,clo))))
	
	((begin? exp)
	 (let loop ((stmts (mapply denest (begin-get-statements exp) #f)) (acc '(cat)))
	   (if (null? stmts)
	       (error 'denest "empty begin" 0)
	       (let ((l (car stmts)))
		 (if (null? (cdr stmts))
		     (make-let-form `(cat ,acc ,(let-form-table l))
				    (let-form-body l))
		     (loop (cdr stmts)
			   `(cat ,acc
				 ,(let-form-table l)
				 (elt (#f ,(let-form-body l))))))))))

	((application? exp)
	 (let ((args (mapply denest (application-get-args exp) #t)))
	   (make-let-form `(cat . ,(map let-form-table args))
			  `(app . ,(map let-form-body args)))))

	((let? exp)
	 (let ((bindings (let-get-bindings exp))
	       (body (let-get-body exp)))
	   (let ((body^ (denest body #f)))
	     (make-let-form `(cat (cat . ,(mapply denest-let-binding bindings))
				  ,(let-form-table body^))
			    (let-form-body body^)))))

	((letrec? exp)
	 (let ((bindings (letrec-get-bindings exp))
	       (body (letrec-get-body exp)))
	   (let ((body^ (denest body #f)))
	     (make-let-form
	      `(cat (cat . ,(mapply denest-letrec-binding-make-closure bindings))
		    (cat . ,(mapply denest-letrec-binding-setup-env bindings))
		    ,(let-form-table body^))
	      (let-form-body body^)))))

	(else (error 'denest-exp "unknown data" exp))))

(define (denest-let-binding b)
  (let ((var (car b))
	(exp (cadr b)))
    (let ((l (denest-exp exp)))
      `(cat ,(let-form-table l)
	    (elt (,var ,(let-form-body l)))))))

(define (denest-letrec-binding-make-closure entry)
  (let ((nm (car entry))
	(clo (cadr entry)))
    (unless (closure? clo)
      (error 'denest-letrec-binding-make-closure "not a closure" clo))
    (let ((size (closure-get-size clo))
	  (lbl (closure-get-label clo)))
      `(elt (,nm (allocate-closure ,size ,lbl))))))

(define (denest-letrec-binding-setup-env entry)
  (let ((nm (car entry))
	(clo (cadr entry)))
    (let ((env (closure-get-env clo))
	  (c (box 0)))
      `(cat . ,(mapply (lambda (v)
			 (let ((i (unbox c)))
			   (set-box! c (+ 1 i))
			   `(elt (#f (set-closure! (var tmp ,nm) ,i ,v)))))
		       env)))))

;;

(define (denest-top top)
  (let* ((lbl (car top))
	 (info (cadr top))
	 (nm (caddr top))
	 (num-args (cadddr top))
	 (res (denest^ (cadddr (cdr top)))))
    (list lbl info nm num-args res)))
