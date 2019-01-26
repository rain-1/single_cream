;; REGISTERS
;; - reg:acc
;; - reg:clos

(define (flatten-exp exp into tail? info stk)
  ;;
  ;; NOTE: every use of 'stackframe' will push a
  ;; return address to the stack
  ;;
  ;; so in order for there to be metadata information
  ;; about that label in the case of a crash we need
  ;; to emit an INFORMATION opcode
  ;;
  (cond ((datum? exp)
	 `(cat ,(flatten-datum (datum-get-value exp))
	       ,(move-accum-to into)))

	((var? exp)
	 `(cat ,(flatten-var (var-get-sort exp)
			     (var-get-name exp))
	       ,(move-accum-to into)))

	((if? exp)
	 (let ((test (if-get-test exp))
	       (consequent (if-get-consequent exp))
	       (antecedent (if-get-antecedent exp)))
	   (let ((lbl-skip-then (gensym "skip-then"))
		 (lbl-skip-else (gensym "skip-else")))
	     `(cat ,(flatten-exp test 'reg:acc #f info stk)
		   (elt (branch ,lbl-skip-then))
		   ,(flatten-exp consequent into tail? info stk)
		   (elt (jump ,lbl-skip-else))
		   (elt (label ,lbl-skip-then))
		   ,(flatten-exp antecedent into tail? info stk)
		   (elt (label ,lbl-skip-else))))))

	((application? exp)
	 (let ((f (cadr exp))
	       (args (cddr exp))
	       (return-site (gensym "site")))
	   `(cat ,(if tail?
		      `(cat)
		      (begin
			`(elt (stackframe ,return-site))))
		 (cat . ,(mapply (lambda (arg)
				   `(cat ,(flatten-exp arg 'reg:acc #f info stk)
					 (elt (push))))
				 args))
		 ,(flatten-exp f 'reg:acc #f info stk)
		 ,(if tail?
		      `(elt (shiftback ,(length args)))
		      `(cat))
		 (elt (call))
		 (elt (label ,return-site))
		 ,(move-accum-to into))))

	((let? exp)
	 `(cat (cat . ,(mapply flatten-let-binding (let-get-bindings exp) info stk))
	       ,(flatten-exp (let-get-body exp) into tail? info stk)))

	((allocate-closure? exp)
	 `(cat (elt ,exp)
	       ,(move-clos-to into)))

	((set-closure!? exp)
	 `(cat ,(flatten-exp (set-closure!-get-clo exp) 'reg:acc #f info stk)
	       ,(move-accum-to 'reg:clo)
	       ,(flatten-exp (set-closure!-get-value exp) 'reg:acc #f info stk)
	       (elt (closure-set! ,(set-closure!-get-index exp)))))

	(else (error 'flatten-exp "unknown exp" exp))))

(define (flatten-datum it)
  (cond ((boolean? it)
	 (if it
	     `(elt (datum-true))
	     `(elt (datum-false))))

	((null? it)
	 `(elt (datum-null)))

	((number? it)
	 `(elt (datum-number ,it)))

	((char? it)
	 `(elt (datum-char ,it)))

	((string? it)
	 `(elt (datum-string ,it)))

	((symbol? it)
	 `(elt (datum-symbol ,it)))

	(else (error 'flatten-datum "unknown datum" it))))

(define (flatten-let-binding entry info stk)
  (flatten-exp (cadr entry) `(var loc ,(car entry)) #f info stk))

(define (flatten-var sort name)
  (case sort
    ((glo) `(elt (var-glo ,name)))

    ((env) `(elt (var-env ,name)))

    ((loc) `(elt (var-loc ,name)))

    (else (error 'flatten-var "unknown var" (list 'var sort name)))))

(define (move-reg-to from reg)
  (let ((set-env (if (eq? from 'reg:acc) 'set-env 'clo-set-env))
	(set-loc (if (eq? from 'reg:acc) 'set-loc 'clo-set-loc)))
    (if (eq? reg from)
	`(cat)
	(if (var? reg)
	    (case (var-get-sort reg)
	      ((env) `(elt (,set-env ,(var-get-name reg))))
	      ((loc) `(elt (,set-loc ,(var-get-name reg))))
	      (else (error 'move-reg-to "unknown place 1" reg)))
	    (case reg
	      ((reg:acc)
	       (unless (eq? from 'reg:clo) (error 'move-reg-to "impossible move" 0))
	       `(elt (clo-set-acc)))
	      ((reg:clo)
	       (unless (eq? from 'reg:acc) (error 'move-reg-to "impossible move" 0))
	       `(elt (set-clo-reg)))
	      (else (error 'move-reg-to "unknown place" reg)))))))
(define (move-accum-to reg) (move-reg-to 'reg:acc reg))
(define (move-clos-to reg) (move-reg-to 'reg:clo reg))

;;

;;			(stack-push! stk `(elt (information ,return-site ,info)))


(define (flatten-toplevel top ender tail? stk)
  (let* ((nm (car top))
	 (info (cadr top))
	 (target (caddr top))
	 (tmps (cadddr top))
	 (exp (cadddr (cdr top)))
         (nm-end (gensym "info-end")))
    (stack-push! stk `(elt (information ,nm ,nm-end ,info)))
    `(cat (elt (label ,nm))
	  ,(if (= 0 tmps)
	       '(cat)
	       `(elt (stack-grow ,tmps)))
	  ,(flatten-exp exp 'reg:acc tail? info stk)
	  (elt (label ,nm-end))
	  ,(if target
	       `(elt (set-glo ,target))
	       '(cat))
	  ,ender)))

(define (flatten-code code stk halt)
  (if (null? code)
      '()
      (let* ((def (car code))
	     (next (if (null? (cdr code))
		       #f
		       (caadr code)))
	     (ender (if next
			`(elt (jump ,next))
			`(elt ,halt))))
	(cons (flatten-toplevel def ender #f stk)
	      (flatten-code (cdr code) stk halt)))))

(define (flatten-program code clos halt)
  ;; TODO grow stack at the start
  (let* ((stk (empty-stack))
	 (res `(cat (cat . ,(flatten-code code stk halt))
		    (cat . ,(mapply flatten-toplevel clos '(elt (ret)) #t stk)))))
    (seq->list `(cat (cat . ,(stack-get stk))
		     ,res))))
