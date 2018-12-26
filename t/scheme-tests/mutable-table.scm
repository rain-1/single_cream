(define tbl (box '()))

(define (add-entry! k v)
  (let ((res (assoc k (unbox tbl))))
    (if res
	(set-cdr! res v)
	(set-box! tbl (cons (cons k v) (unbox tbl))))))

(define (show-it)
  (print 'table)
  (for-each print (unbox tbl))
  (newline))

(begin
  (show-it)
  (add-entry! 'foo 55)
  (show-it)
  (add-entry! 'bar 66)
  (add-entry! 'foo 33)
  (add-entry! 'baz 11)  
  (show-it))
