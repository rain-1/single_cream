(define (combs-with-rep k lst)
  (if (= k 0)
      '(())
      (if (null? lst)
	  '()
          (append
           (map
            (lambda (x)
              (cons (car lst) x))
            (combs-with-rep (- k 1) lst))
           (combs-with-rep k (cdr lst))))))

(begin 
  (for-each print (combs-with-rep 2 (cons "iced" (cons "jam" (cons "plain" '())))))
  (newline)
  (display (length (combs-with-rep 3 '(1 2 3 4 5 6 7 8 9 10))))
  (newline)
  #f)

