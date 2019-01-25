;;;; NOT
(define (not b) (if b #f #t))
(define = eq?)
(define char=? =)

(define (compose f g) (lambda (x) (f (g x))))

(define (gensym s)
  (if (string? s)
      (builtin-gensym (string->symbol s))
      (builtin-gensym s)))

;; NUMBERS
(define (zero? x) (= x 0))
(define modulo remainder)
(define (even? x) (= 0 (modulo x 2)))
(define (odd? x) (not (even? x)))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (abs x) (if (negative? x) (- 0 x) x))
(define (min x y) (if (< x y) x y))
(define (max x y) (if (> x y) x y))

;;;; PRINT
(define (print p) (display p) (newline))

;;;; EQUAL
(define (equal? x y)
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
(define (length lxst) (length/acc lxst 0))
(define (length/acc lest acc)
  (if (null? lest)
      acc
      (length/acc (cdr lest) (+ 1 acc))))

(define (list-ref lst i)
  (if (pair? lst)
      (if (= i 0)
	  (car lst)
	  (list-ref (cdr lst) (- i 1)))
      #f ;; XXX ERROR
      ))

(define (list-set! lst i val)
  (if (pair? lst)
      (if (= i 0)
	  (set-car! lst val)
	  (list-set! (cdr lst) (- i 1) val))
      #f ;; XXX ERROR
      ))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (map/2 f l h)
  (if (null? l)
      (if (not (null? h))
	  (error 'map/2 "lists of different length" #f)
	  '())
      (cons (f (car l) (car h))
            (map/2 f (cdr l) (cdr h)))))

(define (for-each f l)
  (if (null? l)
      #f
      (begin
	(f (car l))
        (for-each f (cdr l)))))

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

(define (member elt lst)
  (if (null? lst)
      #f
      (if (equal? elt (car lst))
          #t
          (member elt (cdr lst)))))

(define (fold kons knil lst)
  (if (null? lst)
      knil
      (kons (car lst)
	    (fold kons knil (cdr lst)))))

(define (replicate n elt)
  (if (= n 0) '() (cons elt (replicate (- n 1) elt))))

(define (foldl f a xs)
  (if (null? xs)
      a
      (foldl f (f a (car xs)) (cdr xs))))

(define (minimum xs)
  (if (pair? xs)
      (foldl min (car xs) (cdr xs))
      #f))

(define (maximum xs)
  (if (pair? xs)
      (foldl max (car xs) (cdr xs))
      #f))

(define (sum lst) (foldl + 0 lst))

(define (assoc key table)
  (if (null? table)
      #f
      (if (equal? key (caar table))
	  (car table)
	  (assoc key (cdr table)))))

(define (comparing < f)
  (lambda (x y)
    (< (f x) (f y))))

(define (insert-by < elt rest)
  (if (null? rest)
      (cons elt '())
      (if (< elt (car rest))
          (cons elt rest)
          (cons (car rest) (insert-by < elt (cdr rest))))))

(define (sort-by < list)
  (if (null? list)
      list
      (insert-by < (car list) (sort-by < (cdr list)))))

(define (concat-map func lst)
  (if (null? lst)
      '()
      (append (func (car lst))
              (concat-map func (cdr lst)))))

(define (concatenate lists) (concat-map (lambda (i) i) lists))

(define (copy-list l) (map (lambda (i) i) l))

(define (replicate n elt)
  (if (= n 0) '() (cons elt (replicate (- n 1) elt))))

;;;; STRING
(define (display-string s) (for-each display-char (string->list s)))
(define (print-string s) (display-string s) (newline))

(define (string-length s) (length (string->list s)))
(define (string-ref s i) (list-ref (string->list s) i))


;;;; BOXES
(define (box val) (cons 'box val))
(define (box? val)
  (if (pair? val)
      (if (eq? 'box (car val))
	  #t
	  #f)
      #f))
(define (unbox b) (cdr b))
(define (set-box! b v) (set-cdr! b v) b)
(define (push-box! b val)
 (set-box! b (cons val (unbox b))))


