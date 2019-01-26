(define (pretty-print x) (print x))

(define (print0 port)
  (if port
      (lambda (n)
	(display/port port n)
	(display/port port (integer->char 0)))
      (lambda (n)
	(display n)
	(display (integer->char 0)))))

(define (print1 port)
  (lambda (n)
    (write/port port n)
    (newline/port port)))

;;

(define (load-macros)
  (let ((macros (read-file "tarot-compiler/macros.scm")))
    (for-each load-macro macros)))

(define (compile expr debug files)
  (let* ((__ (when debug (print 'reading-file)))
	 (stdlib (read-file "./standard-functions.sch"))
	 (__ (when debug (for-each print stdlib)))
	 (__ (when debug (newline)))
	
	 (__ (when debug (print 'pass:desugar)))
	 (toplevel-info (make-stack stdlib))
	 (header-info (make-stack '()))
	 (defs (desugar-top expr debug "eval" toplevel-info header-info))
	 (__ (when debug (pretty-print (stack-top toplevel-info))))
	 (__ (when debug (for-each pretty-print defs)))
	 (__ (when debug (newline)))

         (__ (when debug (print 'pass:hoist)))
         (code (empty-queue))
         (clos (empty-stack))
         (__ (mapply hoist-top defs code clos (stack-get toplevel-info)))
         (__ (when debug (for-each pretty-print (queue:top code))))
         (__ (when debug (for-each pretty-print (unbox clos))))
         (__ (when debug (newline)))
	 
	 (__ (when debug (print 'pass:denest)))
	 (code (mapply denest-top (queue:top code)))
	 (clos (mapply denest-top (stack-get clos)))
	 (__ (when debug (for-each pretty-print code)))
	 (__ (when debug (for-each pretty-print clos)))
	 (__ (when debug (newline)))

	 (__ (when debug (print 'pass:tmp-alloc)))
	 (code (mapply tmp-alloc-top code))
	 (clos (mapply tmp-alloc-top clos))
	 (__ (when debug (pretty-print code)))
	 (__ (when debug (pretty-print clos)))
	 (__ (when debug (newline)))

	 (__ (when debug (print 'pass:flatten)))
	 (code (flatten-program code clos '(halt)))
	 (__ (when debug (pretty-print code)))
	 (__ (when debug (newline)))

	 (__ (when debug (print 'pass:assemble)))
	 (code (assemble code))
	 (__ (when debug (pretty-print code)))
	 (__ (when debug (newline)))
	 )
    
    (if debug
	(newline)
	(files (lambda (qport hport)
		 (for-each (print0 qport) code)
		 (when hport
		   (for-each (print1 hport) (stack-get header-info))))))))

(define (debug-compile-file filename)
  (compile `(include ,filename) #t #f))

(define (compile-file filename)
  (compile `(include ,filename) #f
	   (lambda (thunk)
	     (let* ((filename^ (remove-suffix filename ".scm"))
		    (dotq (add-suffix filename^ ".q"))
		    (doth (add-suffix filename^ ".sch"))
		    (dotq-port (open-output-file dotq))
		    (doth-port (open-output-file doth)))
;;	       (print `(filename ,filename removed ,filename^))
	       (thunk dotq-port doth-port)))))

