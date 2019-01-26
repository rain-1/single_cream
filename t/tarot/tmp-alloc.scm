;; this is the most basic "dumb" allocator possible
;; it simply makes a new cell for each tmp,
;; it doesn't do any lifetime analysis

(define (tmp-index tmp base tmps)
  (+ base (index tmp (queue:top tmps))))

(define (tmp-alloc exp base tmps)
  (cond ((datum? exp) exp)

	((var? exp)
	 (if (eq? 'tmp (var-get-sort exp))
	     (cond ((tmp-index (var-get-name exp) base tmps)
		    => (lambda (i) `(var loc ,i)))
		   (else (error 'tmp-alloc "unbound tmp variable" exp)))
	     exp))

	((if? exp)
	 `(if ,(tmp-alloc (if-get-test exp) base tmps)
	      ,(tmp-alloc (if-get-consequent exp) base tmps)
	      ,(tmp-alloc (if-get-antecedent exp) base tmps)))

	((allocate-closure? exp) exp)

	((set-closure!? exp)
	 (let ((clo (set-closure!-get-clo exp))
	       (index (set-closure!-get-index exp))
	       (value (set-closure!-get-value exp)))
	   `(set-closure! ,(tmp-alloc clo base tmps)
			  ,index
			  ,(tmp-alloc value base tmps))))

	((let? exp)
	 (let ((tbl (mapply tmp-alloc-let-binding
			    (let-get-bindings exp)
			    base
			    tmps))
	       (body (let-get-body exp)))
	   `(let ,tbl ,(tmp-alloc body base tmps))))

	((application? exp)
	 `(app . ,(mapply tmp-alloc (cdr exp) base tmps)))

	(else (error 'tmp-alloc "unknown data" exp))))

(define (tmp-alloc-let-binding entry base tmps)
  (let ((result (tmp-alloc (cadr entry) base tmps)))
    (queue-push! tmps (car entry))
    (list (tmp-index (car entry) base tmps) result)))

;;

(define (tmp-alloc-top top)
  (let* ((lbl (car top))
	 (info (cadr top))
	 (nm (caddr top))
	 (num-args (cadddr top))
	 (tmps (empty-queue))
	 (res (tmp-alloc (cadddr (cdr top)) num-args tmps)))
    `(,lbl
      ,info
      ,nm
      ,(length (queue:top tmps))
      ,res)))
