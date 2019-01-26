(define (arity-check app arity-table filename)
  (when (symbol? (car app))
    (cond ((assoc (car app) arity-table) =>
	   (lambda (entry)
	     ;; entry ::= (<name> <file> <arity>)
	     (let ((arity (caddr entry)))
	       (unless (or (not arity) (= arity (length (cdr app))))
		 (begin (print `((expected ,arity)
			  (got ,(length (cdr app)))
			  (call ,app)
			  (location ,filename)))
			(error 'arity-check "failed!"
			       `((expected ,arity)
				 (got ,(length (cdr app)))
				 (call ,app)
				 (location ,filename)))))))))))

;;(define-record scope (tmp loc env glo captures))
(define (make-scope tmp loc env glo captures)
  (list 'scope tmp loc env glo captures))

(define (scope-tmp s) (list-ref s 1))
(define (scope-loc s) (list-ref s 2))
(define (scope-env s) (list-ref s 3))
(define (scope-glo s) (list-ref s 4))
(define (scope-captures s) (list-ref s 5))

(define (clone-scope s)
  ;; NOTE: clone scope should actually be used everywhere
  ;; but since our code merges scopes we only NEED to do it
  ;; at IF branches
  (make-scope (make-stack (stack-get (scope-tmp s)))
              (scope-loc s)
              (scope-env s)
              (scope-glo s)
              (scope-captures s)))

;; tmp - a stack of "temporary" LET bindings
;; loc - a list of local bindings (function arguments)
;; env - a list of variables available through the env
;; glo - an assoc list (<name> <file> <arity>) of global toplevel variables
;; captures - a queue of closure captured variables

(define (capture! var scope)
  ;; extend the captures with this variable and return (var env i)
  (queue-push! (scope-captures scope) var)
  `(var env ,(index var (queue:top (scope-captures scope)))))

(define (make-var sort) (lambda (i) `(var ,sort ,i)))

;;

(define (hoist-variable var scope filename)
  (cond ((member var (stack-get (scope-tmp scope))) `(var tmp ,var))
	((index var (scope-loc scope)) => (make-var 'loc))
	((member var (scope-env scope))
	 (cond ((index var (queue:top (scope-captures scope))) => (make-var 'env))
	       (else (capture! var scope))))
	((assoc var (scope-glo scope)) `(var glo ,var))
	(else (error (string-append "hoist-variable " (symbol->string var))
 "unbound variable error"
		     `((var ,var) (scope ,scope) (location ,filename))))))

(define (hoist exp scope stk filename)
;(print (cons 'debug-hoist exp))
  (cond ((datum? exp) exp)

	((symbol? exp) (hoist-variable exp scope filename))

	((if? exp)
	 `(if ,(hoist (if-get-test exp) scope stk filename)
	      ,(hoist (if-get-consequent exp) (clone-scope scope) stk filename)
	      ,(hoist (if-get-antecedent exp) (clone-scope scope) stk filename)))

	((lambda? exp)
	 (let ((vars (lambda-get-vars exp))
	       (body (lambda-get-body exp)))
	   (let* ((captures^ (empty-queue))
		  (scope^ (make-scope (empty-stack)
				      vars
				      (append (stack-get (scope-tmp scope))
					      (append (scope-loc scope)
					              (scope-env scope)))
				      (scope-glo scope)
				      captures^))
		  (body^ (hoist body scope^ stk filename))
		  (label (gensym "closure")))
	     (stack-push! stk (list label filename #f (length vars) body^))
	     `(closure ,(length (queue:top captures^))
		       ,(mapply hoist (queue:top captures^) scope stk filename)
		       ,label))))

	((let? exp)
	 (let ((table (mapply (lambda (entry)
				(let ((result (list (car entry) (hoist (cadr entry) scope stk filename))))
				  (stack-push! (scope-tmp scope) (car entry))
				  result))
			      (let-get-bindings exp))))
	   `(let ,table
	      ,(hoist (let-get-body exp) scope stk filename))))

	((letrec? exp)
	 (for-each (lambda (entry)
		     (stack-push! (scope-tmp scope) (car entry)))
		   (letrec-get-bindings exp))
	 (let ((table (mapply (lambda (entry)
				(list (car entry) (hoist (cadr entry) scope stk filename)))
			      (letrec-get-bindings exp))))
	   `(letrec ,table
	      ,(hoist (letrec-get-body exp) scope stk filename))))

	((begin? exp)
	 `(begin . ,(mapply hoist (cdr exp) scope stk filename)))

	((application? exp)
	 (let ((head (hoist (cadr exp) scope stk filename)))
	   (when (and (var? head)
		      (eq? 'glo (var-get-sort head)))
	     (arity-check (cdr exp) (scope-glo scope) filename))
	   `(app ,head . ,(mapply hoist (cddr exp) scope stk filename))))

	(else (error 'hoist "unknown data" exp))))

;;

(define (hoist-def def que stk globals)
  (unless (def? def)
    (error 'hoist-def "not a definition" def))
  (let* ((info  ;(def-get-filename def)
	  (string-append
	   (symbol->string (def-get-name def))
	   (string-append ": " (or (def-get-filename def) "***"))))
	 (scope (make-scope (empty-stack) '() '() globals (empty-queue)))
	 (body (hoist (def-get-body def) scope stk
		      info))
	 (label (gensym "def")))
    (queue-push! que (list label info (def-get-name def) 0 body))
    #t))

(define (hoist-top top que stk globals)
  (cond ((def? top)
	 (hoist-def top que stk globals))
	((raw? top)
	 (let* ((info (raw-get-filename top))
		(scope (make-scope (empty-stack) '() '() globals (empty-queue)))
		(body (hoist (raw-get-body top) scope stk
			     info))
		(label (gensym "raw")))
	   (queue-push! que (list label info #f 0 body)))
	 #t)
	(else (error 'hoist-top "unknown form" top))))

;; these functions emit hoisted code into the stack and queue in the form
;;
;; (<label> <def-name/#f> <num-args> <hoisted>)
