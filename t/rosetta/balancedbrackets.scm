(define (balanced-brackets string)
  (print (b (string->list string) 0)))

(define (boolean-and b1 b2)
  (if b1
      b2
      #f))

(define (b chars sum)
  (if (boolean-and (null? chars) (= 0 sum))
      #t
      (if (null? chars)
          #f
          (if (char=? #\[ (car chars))
              (b (cdr chars) (+ sum 1))
              (if (= sum 0)
		  #f
		  (b (cdr chars) (- sum 1)))))))

(begin
  (balanced-brackets "")
  
  (balanced-brackets "[]")
  (balanced-brackets "[][]")
  (balanced-brackets "[[][]]")
  
  (balanced-brackets "][")
  (balanced-brackets "][][")
  (balanced-brackets "[]][[]"))

