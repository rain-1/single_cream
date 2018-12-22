(define (catalan m)
  (catalan/loop m 1 0))

(define (catalan/loop m c n)
  (if (not (= n m))
      (begin
        (display n)
	(display-string ": ")
	(display c)
	(newline)
        (catalan/loop m (* (quotient (* 2 (- (* 2 (+ n 1)) 1)) (+ (+ n 1) 1)) c) (+ n 1)))
      #f))

(catalan 15)
