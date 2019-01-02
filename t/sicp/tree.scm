(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(print (count-leaves '(1 (2 (3 (4 (5 (6 7))))))))

(print (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
                     10))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(print (subsets '(1 2 3)))

(define (flatmap f l)
  (if (null? l)
      '()
      (append (f (car l)) (flatmap f (cdr l)))))
(define (remove x s)
  (if (null? s)
      '()
      (if (equal? x (car s))
          (cdr s)
          (cons (car s) (remove x (cdr s))))))
(define (permutations s)
  (if (null? s)                    ; empty set?
      (list '())                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(print (permutations '(x y z eeee)))
