(defmacro values
  (lambda (exp)
    `(list . ,(cdr exp))))

(define (make-let-values n vars val body)
  (if (= n 1)
      `(let ((,(car vars) (car ,val))) . ,body)
      (if (= n 2)
	  `(let ((,(car vars) (car ,val))
		 (,(cadr vars) (cadr ,val)))
	     . ,body)
	  (if (= n 3)
	      `(let ((,(car vars) (car ,val))
		     (,(cadr vars) (cadr ,val))
		     (,(caddr vars) (caddr ,val)))
		 . ,body)
	      `(error 'no-let-values)))))

(defmacro let-values
  (lambda (exp)
    ;; (let-values ((<vars> <val>) ...)
    ;;   <body> ...)
    ;; but we only implement the single version
    ;; (let-values ((<vars> <val>))
    ;;   <body> ...)
    (let ((vars (car (car (cadr exp))))
	  (val (cadr (car (cadr exp))))
	  (body (cddr exp))
	  (val-g (gensym 'val)))
      `(let ((,val-g ,val))
	 ,(make-let-values (length vars)
			   vars val-g
			   body)))))


;; utils


(define (assp p l)
  (if (null? l)
      #f
      (if (p (caar l))
	  (car l)
	  (assp p (cdr l)))))

(define (filter p l)
  (if (null? l)
      '()
      (if (p (car l))
	  (cons (car l) (filter p (cdr l)))
	  (filter p (cdr l)))))

(define (any p l)
  (if (null? l)
      #f
      (or (p (car l)) (any p (cdr l)))))

(define (concat-map f l)
  (concatenate (map f l)))

(define (intersect l1 l2)
  (if (null? l1)
      '()
      (let ((x (car l1)))
	(if (member x l2)
	    (cons x (intersect (cdr l1) l2))
	    (intersect (cdr l1) l2)))))


;; variables

;; Variable representation is a vector of a single int
;; it's useful to have an ordering to speed up some set operations

(define (var c) (list 'var c))
(define (var? x) (and (pair? x) (eq? 'var (car x))))
(define (var=? x1 x2) (= (cadr x1) (cadr x2)))
(define (var< i j) (< (cadr i) (cadr j)))
(define (var->int v) (cadr v))


;; substitution

(define (substitution-get k s)
  (cond
   ((assoc k s) => (lambda (p) (values #t (cdr p))))
   (else (values #f #f))))

(define (substitution-set k v s)
  (cons (cons k v) s))

(define substitution-size length)

(define empty-substitution '())



;;; kanren

(define (make-kanren ctr sub dis) (list 'kanren ctr sub dis))
(define (kanren? k) (and (pair? k) (eq? 'kanren (car k))))
(define (counter k) (cadr k))
(define (substitution k) (caddr k))
(define (disequality-store k) (cadddr k))

(define initial-kanren
  (make-kanren 0 empty-substitution '()))


;; Using these modified-* functions instead of make-kanren
;; make-kanren let us add new fields without having to
;; change existing code

(define (modified-counter f k)
  (make-kanren (f (counter k))
               (substitution k)
               (disequality-store k)))

(define (modified-substitution f k)
  (make-kanren (counter k)
               (f (substitution k))
               (disequality-store k)))

(define (modified-disequality-store f k)
  (make-kanren (counter k)
               (substitution k)
               (f (disequality-store k))))

;; streams

;; A stream is one of
;;  * nil
;;  * a pair whose cdr is a stream
;;  * a zero arg lambda 'delaying' a pair or nil
;;    (can't delay twice!)

(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (stream-map f s)
  (cond ((null? s) s)
	((pair? s) (cons (f (car s)) (stream-map f (cdr s))))
	((procedure? s)
	 (lambda ()
	   (stream-map f (s))))))

(define (stream-take n s)
  (if (= n 0)
      '()
      (cond ((null? s) s)
	    ((pair? s) (cons (car s) (stream-take (- n 1) (cdr s))))
	    ((procedure? s)
	     (lambda ()
	       (stream-take n (s)))))))

(define (take n $)
  (if (zero? n)
      '()
      (let (($ (pull $)))
	(if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $)
        '()
        (cons (car $) (take-all (cdr $))))))



;; monad

;; The monad for fair search that carries the kanren around

(define mzero '())
(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus $2 ($1))))
   (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (unit k) (cons k mzero))
(define (bind $ g)
  (cond
   ((null? $) mzero)
   ((procedure? $) (lambda () (bind ($) g)))
   (else (mplus (g (car $)) (bind (cdr $) g)))))

;;;

(define (mapm f l)
  (if (null? l)
      (unit '())
      (bind (f (car l))
            (lambda (v)
              (bind (mapm f (cdr l))
                    (lambda (vs)
                      (unit (cons v vs))))))))



;; micro

(define (one+ n) (+ 1 n))

(define (call/fresh f)
  (lambda (k)
    ((f (var (counter k))) (modified-counter one+ k))))

(define (disj g1 g2) (lambda (k) (mplus (g1 k) (g2 k))))
(define (conj g1 g2) (lambda (k) (bind (g1 k) g2)))


;; mini


(defmacro Zzz
  ;;(_ g)
  (lambda (exp)
    (let ((g (cadr exp))
	  (k (gensym 'k)))
      `(lambda (,k) (lambda () (,g ,k))))))

(defmacro conj+
  (lambda (exp)
    (if (null? (cddr exp))
	(let ((g (cadr exp)))
	  ;;(_ g)
	  `(Zzz ,g))
	(let ((g0 (cadr exp))
	      (g (cddr exp)))
	  ;;(_ g0 g ...)
	  `(conj (Zzz ,g0) (conj+ . ,g))))))

(defmacro disj+
  (lambda (exp)
    (if (null? (cddr exp))
	(let ((g (cadr exp)))
	  ;;(_ g)
	  `(Zzz ,g))
	(let ((g0 (cadr exp))
	      (g (cddr exp)))
	  ;;(_ g0 g ...)
	  `(disj (Zzz ,g0) (disj+ . ,g))))))

(defmacro fresh
  (lambda (exp)
    (let ((vars (cadr exp))
	  (g (cddr exp)))
      (if (null? vars)
	  `(conj+ . ,g)
	  `(call/fresh
	    (lambda (,(car vars))
	      (fresh ,(cdr vars) . ,g)))))))

(defmacro conde
  (lambda (exp)
    `(disj+ . ,(map (lambda (gs) `(conj+ . ,gs)) (cdr exp)))))

;; unification

(define (walk u s)
  ;; Walking a variable or term in a substitution will
  ;; give either the value it points to, or a fresh variable
  ;;
  ;; it is sort of like `weak-head normal form`
  (if (var? u)
      (let-values (((v? v) (substitution-get (var->int u) s)))
	(if v?
	    (walk v s)
	    u))
      u))

(define (walk* v s)
  ;; UNCHANGED
  ;; walk* recursively walks a term to put it into a
  ;; normalized/completely evaluated form
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s)
                      (walk* (cdr v) s)))
     (else v))))

(define (occurs-check x v s)
  ;; UNCHANGED
  ;; Performing occurs check of a variable in a term
  ;; given a substitution.
  ;; This lets us fail on cyclic/unfounded unifications
  (let ((v (walk v s)))
    (cond
     ((var? v) (var=? v x))
     ((pair? v) (or (occurs-check x (car v) s)
                    (occurs-check x (cdr v) s)))
     (else #f))))

(define (extend-substitution/prefix x v s p)
  (if (occurs-check x v s)
      (values #f
              #f)
      (values (substitution-set (var->int x) v s)
              `((,x . ,v) . ,p))))

(define (unify u v s)
  ;; UNCHANGED
  (let-values (((s p) (unify/prefix u v s))) s))

  ;; UNCHANGED
(define (unify/prefix u v s) (unify/prefix* u v s '()))

(define (unify/prefix* u v s p)
  ;; UNCHANGED
  ;; This version of unification builds up a `prefix`
  ;; which contains all the variables that were involved
  ;; in unification that are no longer fresh
  ;;
  ;; This is not needed for pure minikanren but it is
  ;; useful for implementing constraints.
  (let ((u (walk u s)) (v (walk v s)))
    (cond
     ((and (var? u) (var? v) (var=? u v)) (values s p))
     ((var? u) (extend-substitution/prefix u v s p))
     ((var? v) (extend-substitution/prefix v u s p))
     ((and (pair? u) (pair? v))
      (let-values (((s p) (unify/prefix* (car u) (car v) s p)))
	(if s
            (unify/prefix* (cdr u) (cdr v) s p)
            (values #f #f))))
     (else (if (eq? u v)
               (values s p)
               (values #f #f))))))



;; eqeq-diseq, disequality

;; The disequality store d is of the form:
;;
;;      (AND (OR (=/= ...) ...)
;;           (OR (=/= ...) ...) ...)
;;
;; by de-morgan this can be interpreted as:
;;
;; (NOT (OR (AND (== ...) ...)
;;          (AND (== ...) ...) ...))
;;
;; so to normalize such a structure we can
;; normalize each AND, that is the job of
;; the helper function `disequality/assoc`

(define (disequality u v s)
  ;; The `disequality` procedure is sort of the
  ;; dual of `unify`.
  ;;
  ;; If fails (returns #f) if the objects are
  ;; already equal.
  ;;
  ;; If they can never be equal it will return ()
  ;;
  ;; It is defined in terms of a more generalized
  ;; version that takes a big OR assoc list of
  ;; objects to "disunify".

  ;; The way with triangular substitutions was to subtract
  ;; Using unify/prefix means we don't need to subtract
  ;;
  ;; (define (disequality u v s)
  ;;   (let ((s^ (unify u v s)))
  ;;     (if s^
  ;; 	(let ((d (subtract-s s^ s)))
  ;; 	  (if (null? d) #f d))
  ;; 	'())))

  ;; Originally disequality was similar to unify just
  ;; taking u,v.. but it is more useful to take a whole
  ;; assoc list
  ;; 
  ;; (define (disequality u v s)
  ;;   (let-values (((s^ p) (unify/prefix u v s)))
  ;;     (if s^
  ;;         (if (null? p) #f p)
  ;; 	'())))
  
  (disequality/assoc (list (cons u v)) s))

(define (disequality/assoc e s)
  ;; disequality/assoc is functionally equivalent
  ;; to (disequality (map car e) (map cdr e) s)
  ;;
  ;; so if e is () then it returns #f
  ;;
  ;; the way the recursion works is to consider case
  ;; lets say e = ((x . y) . es) then
  ;; * if x and y are already equal then
  ;;   it's all down to es
  ;; * if x and y are distinct then
  ;;   we can return ()
  ;; * if x and y unify then we get a prefix
  ;;   which we consider an OR, we'll append it
  ;;   to ds
  ;;
  (let loop ((e e) (ds #f))
    (if (null? e)
        ds
        (let ((u (caar e)) (v (cdar e)))
          (let-values (((s^ p) (unify/prefix u v s)))
            (if s^
                (if (null? p)
                    (loop (cdr e) ds)
                    (loop (cdr e) (if ds (append p ds) p)))
                '()))))))

(define (normalize-disequality-store k)
  (bind (mapm (lambda (e)
                (let ((d^ (disequality/assoc e (substitution k))))
                  (if d^ (unit d^) mzero)))
              (disequality-store k))
        (lambda (d)
          (unit (modified-disequality-store
                 (lambda (_)
                   ;; you could return d here..
                   ;; the filter is just to remove (or)'s
                   ;; from the reified constraint store
                   (filter (lambda (e) (not (null? e))) d))
                 k)))))

(define (=/= u v)
  (lambda (k)
    (let ((d^ (disequality u v (substitution k))))
      (if d^
          (if (null? d^)
              (unit k)
              (unit (modified-disequality-store
                     (lambda (_)
                       (cons d^ (disequality-store k)))
                     k)))
          mzero))))

(define (== u v)
  (lambda (k)
    (let-values (((s p) (unify/prefix u v (substitution k))))
      (if s
	  (normalize-disequality-store
           (modified-substitution (lambda (_) s) k))
	  mzero))))



;; reification

(define (reify-name n)
  ;; XXX TODO
;  (string->symbol
;   (string-append "_" "." (number->string n))))
  (list '_. n))

(define (reify-s v s)
  ;; Given a term and substitution this extends
  ;; the substitution with nice names for each
  ;; fresh variable in v
  (let ((v (walk v s)))
    (cond
     ((var? v) (let ((n (reify-name (substitution-size s))))
                 (substitution-set (var->int v) n s)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-term t s)
  ;; To reify a term:
  ;;
  ;; First use walk* so that we have a flat term
  ;; containing only fresh variables
  ;;
  ;; Then extend our substitution with good names
  ;; for all those variables
  ;;
  ;; walk* it again to reify all the fresh variables
  ;; in the term itself.
  ;;
  ;; Doing it in two steps like this means we use up
  ;; names for any variables that aren't in the final
  ;; term
  (let ((v (walk* t s)))
    (walk* v (reify-s v empty-substitution))))

(define (make-=/= eq) `(=/= ,(car eq) ,(cdr eq)))
(define (reify-kanren k)
  ;; The query variable will always be the very first
  ;; one so reify (var 0) along with all the constraints
  ;; or extra conditions that might need to be displayed
  (let ((constraints (map (lambda (d) `(or . ,(map make-=/= d)))
			  (disequality-store k))))
    (reify-term `(,(var 0) where
		  . ,constraints)
		(substitution k))))

;; run

(define (run^ n g)
  ;; Compute up to a set limit of results
  (map reify-kanren (take n ((call/fresh g) initial-kanren))))

(define (run* g)
  ;; Compute every result
  (map reify-kanren (take-all ((call/fresh g) initial-kanren))))


;; prelude

(define membero
  (lambda (x l)
    (conde
     ((fresh (d)
        (== (cons x d) l)))
     ((fresh (a d)
        (== (cons a d) l)
        (membero x d))))))

(define appendo
  (lambda (l s out)
    (conde
     ((== '() l) (== s out))
     ((fresh (a d res)
        (== `(,a . ,d) l)
        (== `(,a . ,res) out)
        (appendo d s res))))))

(define (eacho p l)
  (conde
   ((== l '()))
   ((fresh (l-car l-cdr)
      (== l `(,l-car . ,l-cdr))
      (p l-car)
      (eacho p l-cdr)))))

(define (mapo f l fl)
  (conde
   ((== l '()) (== fl '()))
   ((fresh (l-car l-cdr
                  fl-car fl-cdr)
      (== l `(,l-car . ,l-cdr))
      (== fl `(,fl-car . ,fl-cdr))
      (f l-car fl-car)
      (mapo f l-cdr fl-cdr)))))

;(unify 3 5 empty-substitution)
;(unify 3 3 empty-substitution)
;(unify '(a b) '(a b) empty-substitution)
;(unify '(a b) '(x b) empty-substitution)
;(unify '(a b) '(a y) empty-substitution)
;(unify (var 3) '(a y) empty-substitution)
;(unify (list (var 3) (var 4)) '(a y) empty-substitution)

;initial-kanren
;(counter initial-kanren)
;(modified-counter one+ initial-kanren)
;(counter (modified-counter one+ initial-kanren))
;(var (counter (modified-counter one+ initial-kanren)))

;(call/fresh (lambda (q) (== 3 3)))
;((call/fresh (lambda (q) (== 3 3))) initial-kanren)
(run* (lambda (q) (== 3 3)))
(run* (lambda (q) (== 3 4)))
(run* (lambda (q) (== 3 q)))
(run* (lambda (q)
        (membero q '(a b c))))
(run* (lambda (q)
	(conde
         ((=/= q 'b)
	  (membero q '(a b c))))))
(run* (lambda (q)
	(fresh (x y)
	       (== q `(,x ,y))
	       (=/= x y)
	       (membero q '((a a) (b a) (b a) (b b) (c b))))))
(run* (lambda (q)
        (fresh (x y)
               (== q `(,x ,y))
               (appendo x y '(a b c)))))

;;; Copyright © 2018 Daniel P. Friedman, William E. Byrd, Oleg Kiselyov, and Jason Hemann
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;; Implementation of the arithmetic system used in 'The Reasoned
;;; Schemer, Second Edition,' by Friedman, Byrd, Kiselyov, and Hemann
;;; (MIT Press, 2018).

;;; Definitions are presented in the order in which they appear in
;;; Chapters 7 and 8.

;;; As in the book, there are three definitions of '/o'.  The first two,
;;; flawed definitions, are commented out using Scheme's '#;' convention.
;;; The final definition of '/o' is uncommented.
;;;
;;; If you wish work through the Chapter 8 one frame at a time, then
;;; please comment out the final definition of '/o' by adding a '#;'
;;; immediately before the '(defrel (/o ...) ...)', and uncomment the
;;; appropriate definition of '/o' as you encounter it while reading
;;; this chapter.

;;; Please be sure to first load 'trs2-impl.scm' before loading this
;;; file.



; Helper definitions from Chapters 2 and 4.
(define (nullo x)
  (== '() x))

(define (conso a d p)
  (== `(,a . ,d) p))

(define (caro p a)
  (fresh (d)
    (== (cons a d) p)))

(define (cdro p d)
  (fresh (a)
    (== (cons a d) p)))

(define (appendo l t out)
  (conde
    ((nullo l) (== t out))
    ((fresh (a d res)
       (conso a d l)
       (conso a res out)
       (appendo d t res)))))



;;; Here are the key parts of Chapter 7
(define (bit-xoro x y r)
  (conde
    ((== 0 x) (== 0 y) (== 0 r))
    ((== 0 x) (== 1 y) (== 1 r))
    ((== 1 x) (== 0 y) (== 1 r))
    ((== 1 x) (== 1 y) (== 0 r))))

(define (bit-ando x y r)
  (conde
    ((== 0 x) (== 0 y) (== 0 r))
    ((== 1 x) (== 0 y) (== 0 r))
    ((== 0 x) (== 1 y) (== 0 r))
    ((== 1 x) (== 1 y) (== 1 r))))


(define (half-addero x y r c)
  (fresh ()
	(bit-xoro x y r)
	(bit-ando x y c)))

; Alternative definition of 'full-addero' from frame 7:15 on page 87.
;
; For performance reasons, we use this explicit table version of
; 'full-addero' (which no longer uses 'half-addero').
(define (full-addero b x y r c)
  (conde
    ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))))


(define (build-num n)
  (cond
    ((zero? n) '())
    ((even? n)
     (cons 0
       (build-num (quotient n 2))))
    ((odd? n)
     (cons 1
       (build-num (quotient (- n 1) 2))))))

(define (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(define (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(define (addero b n m r)
  (conde
    ((== 0 b) (== '() m) (== n r))
    ((== 0 b) (== '() n) (== m r)
     (poso m))
    ((== 1 b) (== '() m)
     (addero 0 n '(1) r))
    ((== 1 b) (== '() n) (poso m)
     (addero 0 '(1) m r))
    ((== '(1) n) (== '(1) m)
     (fresh (a c)
       (== `(,a ,c) r)
       (full-addero b 1 1 a c)))
    ((== '(1) n) (gen-addero b n m r))
    ((== '(1) m) (>1o n) (>1o r)
     (addero b '(1) n r))
    ((>1o n) (gen-addero b n m r))))

(define (gen-addero b n m r)
  (fresh (a c d e x y z)
    (== `(,a . ,x) n)
    (== `(,d . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (full-addero b a d c e)
    (addero e x y z)))

(define (pluso n m k)
  (addero 0 n m k))

(define (minuso n m k)
  (pluso m k n))

;;; Here are the key parts of Chapter 8
(define (*o n m p)
  (conde
    ((== '() n) (== '() p))
    ((poso n) (== '() m) (== '() p))  
    ((== '(1) n) (poso m) (== m p))   
    ((>1o n) (== '(1) m) (== n p))
    ((fresh (x z)
       (== `(0 . ,x) n) (poso x)
       (== `(0 . ,z) p) (poso z)
       (>1o m)
       (*o x m z)))
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)
       (== `(0 . ,y) m) (poso y)
       (*o m n p)))
    ((fresh (x y)
       (== `(1 . ,x) n) (poso x)      
       (== `(1 . ,y) m) (poso y)
       (odd-*o x n m p)))))

(define (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (pluso `(0 . ,q) m p)))

(define (bound-*o q p n m)
  (conde
    ((== '() q) (poso p))
    ((fresh (a0 a1 a2 a3 x y z)
       (== `(,a0 . ,x) q)
       (== `(,a1 . ,y) p)
       (conde
         ((== '() n)
          (== `(,a2 . ,z) m)
          (bound-*o x y z '()))
         ((== `(,a3 . ,z) n) 
          (bound-*o x y z m)))))))

(define (=lo n m)
  (conde
    ((== '() n) (== '() m))
    ((== '(1) n) (== '(1) m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (=lo x y)))))

(define (<lo n m)
  (conde
    ((== '() n) (poso m))
    ((== '(1) n) (>1o m))
    ((fresh (a x b y)
       (== `(,a . ,x) n) (poso x)
       (== `(,b . ,y) m) (poso y)
       (<lo x y)))))

(define (<=lo n m)
  (conde
    ((=lo n m))
    ((<lo n m))))

(define (<o n m)
  (conde
    ((<lo n m))
    ((=lo n m)
     (fresh (x)
       (poso x)
       (pluso n x m)))))

(define (<=o n m)
  (conde
    ((== n m))
    ((<o n m))))

(run* (lambda (q)
        (fresh (x y)
               (== q `(,x ,y))
               (*o x y (build-num 30)))))
