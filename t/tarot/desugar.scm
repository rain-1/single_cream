(define include-stack (empty-stack))

(define (head? atom exp shadow)
  (and (pair? exp)
       (not (member atom shadow))
       (eq? (car exp) atom)))

(define macro-definitions
  (box '()))

(define (load-macro mac)
  (if (head? 'defmacro mac '())
      (push-box! macro-definitions (cons (cadr mac) (eval (caddr mac))))
      (eval mac)))

(define (macro? exp)
  (and (pair? exp)
       (cond ((assoc (car exp) (unbox macro-definitions)) => cdr)
	     (else #f))))

(define (desugar exp shadow)
  (cond ((or (number? exp) (string? exp) (char? exp) (boolean? exp))
	 `(datum ,exp))
	
	((symbol? exp) exp)

	((macro? exp)
	 => (lambda (expander)
	      (desugar (expander exp) shadow)))

	;; named let -> letrec
	((and (head? 'let exp shadow)
	      (>= (length exp) 4)
	      (symbol? (cadr exp)))
	 (let ((name (cadr exp))
	       (table (caddr exp))
	       (body* (cdddr exp)))
	   (let ((vars (map car table))
		 (vals (map cadr table)))
	     `(letrec ((,name ,(desugar `(lambda ,vars . ,body*) (cons name shadow))))
		(app ,name . ,(mapply desugar vals shadow))))))

	((head? 'if exp shadow)
	 (unless (= 4 (length exp))
	   (error 'desugar "malformed if expression" exp))
	 `(if ,(desugar (cadr exp) shadow)
	      ,(desugar (caddr exp) shadow)
	      ,(desugar (cadddr exp) shadow)))

	;; TEMPORORAY
	((head? 'let* exp shadow)
	 (desugar `(let . ,(cdr exp)) shadow))

	((head? 'lambda exp shadow)
	 (desugar-lambda (cadr exp) (cddr exp) shadow))

	((head? 'begin exp shadow)
	 (desugar-begin (mapply desugar (cdr exp) shadow)))

	((head? 'quote exp shadow)
	 (desugar-quote (cadr exp)))

	((or (head? 'let exp shadow)
	     (head? 'letrec exp shadow))
	 (desugar-let (car exp) (cadr exp) (cddr exp) shadow))

	((pair? exp)
	 `(app . ,(mapply desugar exp shadow)))

	(else (error 'desugar "unknown object" exp))))

(define (desugar-begin stmts)
  (cond ((null? stmts) (error 'desugar-begin "null" 0))
	((null? (cdr stmts)) (car stmts))
	(else `(begin . ,stmts))))

(define (desugar-lambda vars body* shadow)
  `(lambda ,vars ,(desugar-begin (mapply desugar body* (append vars shadow)))))

(define (desugar-quote q)
  (cond ((pair? q)
	 `(app cons ,(desugar-quote (car q))
	            ,(desugar-quote (cdr q))))
	(else
	 `(datum ,q))))

(define (desugar-let-binding binding shadow)
  (let ((var (car binding))
	(exp (cadr binding)))
    `(,var ,(desugar exp shadow))))

(define (desugar-let l bindings body* shadow)
;  (print `(desugar-let ,l ,bindings ,body* ,shadow))
  (let ((vars (map car bindings)))
    `(,l ,(mapply desugar-let-binding bindings shadow)
       ,(desugar-begin (mapply desugar body* (append vars shadow))))))

;;

(define (desugar-def def filename stk hstk)
  (unless (and (pair? def) (eq? 'define (car def)))
    (begin (print def)
	   (error 'desugar-def "not a definition" def)))
  (let loop ((def-head (cadr def))
	     (def-body (cddr def)))
    (cond ((symbol? def-head)
	   (when (member def-head '(if lambda begin))
	     (begin (print def)
		    (error 'desugar-def "ridiculous" 0)))
	   (let* ((def-body (desugar (desugar-begin def-body) '()))
                  (header (list def-head filename (estimate-arity def-body))))
	     (stack-push! stk header)
	     (stack-push! hstk header)
	     `(define ,filename ,def-head ,def-body)))
	  ((pair? def-head)
	   ;;
	   ;; (define (foo x y z) ...)
	   ;; ~> (define foo (lambda (x y z) ...)
	   ;;
	   (loop (car def-head) (list `(lambda ,(cdr def-head) . ,def-body))))
	  (else (print def) (error 'desugar-def "bad definition head" def)))))

(define (desugar-top top debug filename stk hstk)
  ;; at the top level we will see either
  ;; (include <filename>)
  ;; (define <name> <body> ...)
  ;; or a raw lisp expression to execute
  (cond ((head? 'include top '())
	 (if (member (cadr top) (stack-get include-stack))
	     '()
	     (let ((filename (cadr top)))
	       (stack-push! include-stack filename)
	       (concatenate (mapply desugar-top (read-file filename) debug filename stk hstk)))))

	((head? 'define top '())
	 (when debug
	   (print `(desugaring def ,top)))
	 (list (desugar-def top filename stk hstk)))
	
	((head? 'defmacro top '())
	 (when debug
	   (print `(loading macro ,top)))
	 (load-macro top)
         '())
	
	(else
	 (when debug
	   (print `(desugaring raw ,top)))
	 (list `(raw ,filename ,(desugar top '()))))))

;;

(define (estimate-arity d)
  ;; TODO: support understanding the arity of things like (define stream-car car) ?
  (if (and (pair? d)
           (eq? (car d) 'lambda))
      (length (cadr d))
      #f))
