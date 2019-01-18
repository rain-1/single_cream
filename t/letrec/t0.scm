(preprocess
 '(letrec ((local-even? (lambda (n)
			  (if (= n 0) #t
                              (local-odd? (- n 1)))))
	   (local-odd? (lambda (n)
			 (if (= n 0) #f
                             (local-even? (- n 1))))))
    (list (local-even? 23) (local-odd? 23))))

