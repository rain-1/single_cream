(define (accumulate f start more)
  (if (null? more)
      start
      (accumulate f (f start (car more)) (cdr more))))

(define (filter p l) (if (null? l) '() (if (p (car l)) (cons (car l) (filter p (cdr l))) (filter p (cdr l)))))

(define (flatmap f l) (accumulate (lambda (start more) (append start (f more))) '() l))

(define (enumerate-interval start stop)
  (if (= start stop)
      (list start)
      (cons start (enumerate-interval (+ start 1) stop))))

;; (define (list-ref l idx)
;;   (if (= idx 0)
;;       (car l)
;;       (list-ref (cdr l) (- idx 1))))

;; (define (abs x) (if (< x 0) (- 0 x) x))

;; stole from http://www.billthelizard.com/2011/06/sicp-242-243-n-queens-problem.html thanks bill

(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))

(define empty-board '())

(define (adjoin-position row col positions)
   (append positions (list (make-position row col))))

(define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
         (other-queens (filter (lambda (q)
                                 (not (= col (position-col q))))
                               positions))
	 (attacks? (lambda (q1 q2)
		     (or (= (position-row q1) (position-row q2))
			 (= (abs (- (position-row q1) (position-row q2)))
			    (abs (- (position-col q1) (position-col q2))))))))
     (let ((iter (lambda (iter q board)
		   (or (null? board)
		       (and (not (attacks? q (car board)))
			    (iter iter q (cdr board)))))))
       (iter iter kth-queen other-queens))))

;;

(define (queen-cols board-size k)
  (if (= k 0)
      (list empty-board)
      (filter
       (lambda (positions) (safe? k positions))
       (flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))
        (queen-cols board-size (- k 1))))))

(define (queens board-size)
  (queen-cols board-size board-size))

(print (length (queens 4)))
(print (length (queens 5)))
(print (length (queens 6)))

;; takes too long to compute to include in automated testing
;(print (length (queens 7)))
;(print (length (queens 8))) ;; takes about 14 MB of memory to compute (result is 92)

;; requires more memory than we have
;(print (length (queens 9)))
;(print (length (queens 10)))

