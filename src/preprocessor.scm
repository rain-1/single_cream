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

(define (unquote? exp)
  (if (pair? exp)
      (eq? 'unquote (car exp))
      #f))

(define (make-cons a b)
  (cons 'cons (cons a (cons b '()))))

(define (make-quote a)
  (cons 'quote (cons a '())))

(define (expand-quasiquote/start exps)
  (if (if (pair? exps)
	  (null? (cdr exps))
	  #f)
      (expand-quasiquote (car exps))
      (error 'bad-quasiquote)))

(define (expand-quasiquote exp)
  ;; TODO: levels
  (if (unquote? exp)
      (cadr exp)
      (if (pair? exp)
	  (make-cons (expand-quasiquote (car exp))
		     (expand-quasiquote (cdr exp)))
	  (make-quote exp))))

(define _ ;; used to do work without printing the result to stdout
  (begin
    (add-macro! 'and (compose expand-and cdr))
    (add-macro! 'or (compose expand-or cdr))
    (add-macro! 'let (compose expand-let cdr))
    (add-macro! 'quasiquote (compose expand-quasiquote/start cdr))
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
		     (preprocess/helper shadow ((cdr m-entry) s))
		     (cons (preprocess/helper shadow (car s))
			   (preprocess/helper shadow (cdr s)))))
	       (assoc (car s) (unbox macro-table)))))
      (if (pair? s)
	  (cons (preprocess/helper shadow (car s))
		(preprocess/helper shadow (cdr s)))
	  s)))

(define (preprocess s)
  (preprocess/helper '() s))

;;;;;

(define (expand-defmacro exps)
  (let ((name (cadr exps))
	(expander (caddr exps)))
    `(define _
       (begin
	 (add-macro! ',name ,expander)
	 #f))))

(define _
  (begin
    (add-macro! 'defmacro expand-defmacro)
    #f))

(defmacro list
  (lambda (t) (fold (lambda (a c) `(cons ,a ,c)) ''() (cdr t))))

(defmacro when
  (lambda (exp)
    (let ((test (cadr exp))
	  (body `(begin . ,(cddr exp))))
      `(if ,test
	   ,body
	   #f))))

(defmacro unless
  (lambda (exp)
    (let ((test (cadr exp))
	  (body `(begin . ,(cddr exp))))
      `(if ,test
	   #f
	   ,body))))

;; redefine LET to support named let, using the Y combinator

(define (y/n n f)
  (let ((g (gensym 'g)))
    (if (= n 1)
	`((lambda (,g) (,g ,g))
	  (lambda (,g)
	    (,f (lambda (a) ((,g ,g) a)))))
	(if (= n 2)
	    `((lambda (,g) (,g ,g))
	      (lambda (,g)
		(,f (lambda (a b) ((,g ,g) a b)))))
	    (if (= n 3)
		`((lambda (,g) (,g ,g))
		  (lambda (,g)
		    (,f (lambda (a b c) ((,g ,g) a b c)))))
		(error 'no-y-combinator))))))

(define (expand-named-let loop vars vals body)
  `(,(y/n (length vars)
	  `(lambda (,loop)
	     (lambda ,vars . ,body)))
    . ,vals))

(defmacro let
  (lambda (exp)
    (if (symbol? (cadr exp))
	(expand-named-let (cadr exp) (map car (caddr exp)) (map cadr (caddr exp)) (cdddr exp))
	(expand-let (cdr exp)))))

;;; COND

(define (cond/0? exp)
  (and (pair? exp) (eq? 'cond (car exp)) (null? (cdr exp))))
(define (cond/else? exp)
  (and (pair? exp)
       (eq? 'cond (car exp))
       (pair? (cdr exp))
       (pair? (cadr exp))
       (eq? 'else (caadr exp))
       (null? (cddr exp))))
(define (cond/else-get-else exp) (cdadr exp))
(define (cond/1? exp)
  (and (pair? exp)
       (eq? 'cond (car exp))
       (pair? (cdr exp))
       (pair? (cadr exp))
       (null? (cdadr exp))))
(define (cond/1-get-one exp) (caadr exp))
(define (cond/1-get-next exp) (cddr exp))
(define (cond/=>? exp)
  (and (pair? exp)
       (eq? 'cond (car exp))
       (pair? (cdr exp))
       (pair? (cadr exp))
       (pair? (cdadr exp))
       (eq? '=> (cadadr exp))
       (pair? (cddadr exp))
       (null? (cdddr (cadr exp)))))
(define (cond/=>-get-test exp) (caadr exp))
(define (cond/=>-get-thunk exp) (caddr (cadr exp)))
(define (cond/=>-get-next exp) (cddr exp))
(define (cond/clause? exp)
  (and (pair? exp)
       (eq? 'cond (car exp))
       (pair? (cdr exp))
       (pair? (cadr exp))))
(define (cond/clause-get-test exp) (caadr exp))
(define (cond/clause-get-rest exp) (cdadr exp))
(define (cond/clause-get-next exp) (cddr exp))

(define (cond-get-next exp)
  `(cond . ,(cddr exp)))

(defmacro cond
  (lambda (exp)
    (if (cond/0? exp)
	`(exit) ;; todo void
	(if (cond/else? exp)
	    `(begin . ,(cond/else-get-else exp))
	    (if (cond/1? exp)
		`(or ,(cond/1-get-one exp) ,(cond-get-next exp))
		(if (cond/=>? exp)
		    (let ((test (cond/clause-get-test exp))
			  (thunk (cond/=>-get-thunk exp))
			  (tmp (gensym 'cond-tmp)))
		      `(let ((,tmp ,test))
			 (if ,tmp
			     (,thunk ,tmp)
			     ,(cond-get-next exp))))
		    (if (cond/clause? exp)
			(let ((test (cond/clause-get-test exp))
			      (rest (cond/clause-get-rest exp)))
			  `(if ,test
			       (begin . ,rest)
			       ,(cond-get-next exp)))
			(error 'cond-bad-syntax))))))))
