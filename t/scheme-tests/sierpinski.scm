(define (loop acc spaces n)
  (if (= 0 n)
      acc
      (loop
       (append
        (map (lambda (x) (append spaces (append x spaces))) acc)
        (map (lambda (x) (append x (append (cons " " '()) x))) acc))
       (append spaces spaces)
       (- n 1))))

(define (sierpinski n)
  (for-each
   (lambda (x) (begin (for-each display-string x) (newline)))
   (loop (cons (cons "*" '()) '()) (cons " " '()) n)))

(sierpinski 3)
(sierpinski 4)
(sierpinski 5)
