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

(define (make-empty-binding var)
  `(,var #f))

(defmacro letrec
  (lambda (exp)
    ;; (letrec ((<var> (lambda <args> . <body>)) ...) . <body>)
    (let ((bindings (cadr exp))
	  (body (cddr exp)))
      (let ((vars (map car bindings))
	    (lambdas (map cadr bindings)))
	;; (let ((<var> #f) ...)
	;;   (set! <var> <lambda>)
	;;   ...
	`(let ,(map make-empty-binding vars)
	   ,(cons 'begin
		  (append (map (lambda (binding)
				 (let ((var (car binding))
				       (lambda^ (cadr binding)))
				   `(set! ,var ,lambda^)))
			       bindings)
			  body)))))))

;; redefine LET to support named let

(define (expand-named-let loop vars vals body)
  `(letrec ((,loop (lambda ,vars . ,body)))
     (,loop . ,vals)))

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

;;; TAROT STUFF

;; redefine OR and AND

(defmacro or
  (lambda (t)
    (if (null? (cdr t))
	#f
	(if (null? (cddr t))
	    (cadr t)
	    (let ((a (cadr t))
		  (b (cddr t))
		  (tmp (gensym 'tmp)))
	      `(let ((,tmp ,a))
		 (if ,tmp ,tmp (or . ,b))))))))

(defmacro and
  (lambda (t)
    (if (null? (cdr t))
	#t
	(if (null? (cddr t))
	    (cadr t)
	    (let ((a (cadr t))
		  (b (cddr t)))
	      `(if ,a (and . ,b) #f))))))
;; <case> ::= (case <exp> <clause> (else <exp>))
;;
;; <clause> ::= ((<thing>) <exp>)

;; (case foo ((x) 1) ((y) 2) (else 3))
;; -->
;; let tmp foo
;; (if (eq? tmp 'x) 1)
;;   ...((y) 2) (else 3))

(define (else-clause? head)
  (and (pair? head)
       (eq? 'else (car head))))

(define (length-1? lst)
 (if (null? lst) #f (if (null? (cdr lst)) #t #f)))

(define (length-2? lst)
 (if (null? lst) #f (length-1? (cdr lst))))

(define (compile-case t clauses)
  (if (null? clauses)
      '(exit)
      (let ((head (car clauses))
            (rest (cdr clauses)))
        (if (else-clause? head)
            (cadr head) ;; TODO: else needs implicit begin
            (let ((test (car head))
                  (body (cdr head)))
              `(if ,(if (length-1? test)
                        `(equal? ,t ',(car test))
                        (if (length-2? test)
                            `(or (equal? ,t ',(car test))
                                 (equal? ,t ',(cadr test)))
                            `(member ,t ',test)))
                   (begin . ,body)
                   ,(compile-case t rest)))))))

(defmacro case
  (lambda (exp)
    (let ((discriminant (cadr exp))
          (tmp (gensym 'tmp)))
      `(let ((,tmp ,discriminant))
         ,(compile-case tmp (cddr exp))))))

(defmacro mapply
  (lambda (exp)
    ;;(mapply f xs arg ...)
    (let ((f (cadr exp))
	  (xs (caddr exp))
	  (args (cdddr exp))
	  (x (gensym "x")))
      `(map (lambda (,x) (,f ,x . ,args)) ,xs))))

(defmacro inc!
  (lambda (form)
    (let ((x (cadr form)))
      `(set-box! ,x (+ (unbox ,x) 1)))))

(defmacro dec!
  (lambda (form)
    (let ((x (cadr form)))
      `(set-box! ,x (- (unbox ,x) 1)))))

(define (make-let* bindings body)
  (if (null? bindings)
      `(begin
	 . ,body)
      (if (null? (cdr bindings))
	  `(let (,(car bindings))
	     . ,body)
	  `(let (,(car bindings))
	     ,(make-let* (cdr bindings) body)))))

(defmacro let*
  (lambda (form)
    ;; `(let <bindings> . <body>)
    (let ((bindings (cadr form))
	  (body (cddr form)))
      (make-let* bindings body))))

;; some code uses arity 1 ERROR
;; other code uses arity 3 ERROR inherited from Chez
;; this hack works around it
(defmacro error
  (lambda (form)
    (if (null? (cddr form))
	`(builtin-error ,(cadr form))
	`(builtin-error ,(caddr form)))))
