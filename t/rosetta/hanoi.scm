(define (hanoi n a b c)
  (if (<= n 0)
    #f
    (begin
      (hanoi (- n 1) a c b)
      (display-string "Move disk from pole ")
      (display a)
      (display-string " to pole ")
      (display b)
      (newline)
      (hanoi (- n 1) c b a))))
 
(hanoi 4 1 2 3)
