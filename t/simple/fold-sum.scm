(fold + 0 '(1 2 3 4 5))
(fold + 0 '(4 6 2 1 8 4 9))

(define (snds lst)
 (car
  (fold (lambda (x ys)
          (cons (cons x (cdr ys))
                (car ys)))
        (cons '() '())
        lst)))

(define (preds lst)
  (fold (lambda (x ys)
          (cons '()
            (map (lambda (zs) (cons x zs)) ys)))
        '()
         lst))

(define (frev lst)
  ((fold (lambda (x ys)
           (lambda (i)
            (ys (cons x i))))
         (lambda (i) i)
         lst)
   '()))

(snds '(1 2 3 4 5))
(preds '(1 2 3 4 5))
(frev '(1 2 3 4 5))

