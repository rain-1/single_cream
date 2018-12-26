(define macro-table (box '()))

(define (add-assoc-entry! tbl k v)
  ((lambda (res) 
     (if res
	 (set-cdr! res v)
	 (set-box! tbl (cons (cons k v) (unbox tbl)))))
   (assoc k (unbox tbl))))

(define (add-macro! nm mac) (add-assoc-entry! macro-table nm mac))

;;;;

(define (make-if t c a)
  (cons 'if (cons t (cons c (cons a '())))))

(define (make-lambda args body)
  (cons 'lambda (cons args body)))

(define (expand-and exps)
  (if (null? exps)
      #t
      (make-if (car exps)
               (expand-and (cdr exps))
               #f)))

(define (expand-or exps)
  (if (null? exps)
      #f
      (make-if (car exps)
               #t
               (expand-or (cdr exps)))))

(define (expand-let exps)
  ((lambda (args vals body)
     (cons (make-lambda args body) vals))
   (map car (car exps))
   (map cadr (car exps))
   (cdr exps)))

(define _ ;; used to do work without printing the result to stdout
  (begin
    (add-macro! 'and expand-and)
    (add-macro! 'or expand-or)
    (add-macro! 'let expand-let)
    #f))

;;;;

(define (preprocess/helper shadow s)
  (if (if (pair? s)
	  (if (symbol? (car s))
	      (not (member (car s) shadow))
	      #f)
	  #f)
      (if (eq? (car s) 'quote)
	  s
	  (if (eq? (car s) 'lambda)
	      (make-lambda (cadr s)
			   (preprocess/helper (append (cadr s) shadow)
					      (cddr s)))
	      ((lambda (m-entry)
		 (if m-entry
		     (preprocess/helper shadow ((cdr m-entry) (cdr s)))
		     (cons (preprocess/helper shadow (car s))
			   (preprocess/helper shadow (cdr s)))))
	       (assoc (car s) (unbox macro-table)))))
      (if (pair? s)
	  (cons (preprocess/helper shadow (car s))
		(preprocess/helper shadow (cdr s)))
	  s)))

(define (preprocess s)
  (preprocess/helper '() s))
