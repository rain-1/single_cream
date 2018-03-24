;;;; NOT
(define (not b) (if b #f #t))
(define = eq?)

;;;; PRINT
(define (print p) (display p) (newline))

;;;; EQUAL
(define (equal? x y)
;; TODO: string equality
  (print 'equal?)
  (print x)
  (print y)
  (if (pair? x)
      (if (pair? y)
	  (if (equal? (car x) (car y))
	      (equal? (cdr x) (cdr y))
	      #f)
	  #f)
      (eq? x y)))


;;;; CXR
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (cadddr x) (car (cdddr x)))
(define (cddddr x) (cdddr (cdr x)))
(define (cadadr x) (cadar (cdr x)))
(define (cddadr x) (cdr (cdr (car (cdr x)))))

;;;; LIST
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (revappend l r)
  (if (null? l)
      r
      (revappend (cdr l) (cons (car l) r))))

(define (reverse l) (revappend l '()))

(define (filter p l)
  (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (filter p (cdr l)))
          (filter p (cdr l)))))
