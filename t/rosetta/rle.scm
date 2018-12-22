;(define (run-length-decode v)
;  (apply string-append (map (lambda (p) (make-string (car p) (cdr p))) v)))

(define string-length length)
(define string-ref list-ref)

(define (run-length-encode s)
  ((lambda (n)
     (run-length-encode/loop s (- n 2) (string-ref s (- n 1)) 1 '()))
   (string-length s)))

(define (run-length-encode/loop s i c k v)
  (if (negative? i)
      (cons (cons k c) v)
      ((lambda (x)
	 (if (char=? c x)
	     (run-length-encode/loop s (- i 1) c (+ k 1) v)
             (run-length-encode/loop s (- i 1) x 1 (cons (cons k c) v))))
       (string-ref s i))))

(run-length-encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
;; ((12 . #\W) (1 . #\B) (12 . #\W) (3 . #\B) (24 . #\W) (1 . #\B) (14 . #\W))
