(define Y/1                 ; (Y f) = (g g) where
  (lambda (f)             ;         (g g) = (f  (lambda a (apply (g g) a)))
    ((lambda (g) (g g))   ; (Y f) ==        (f  (lambda a (apply (Y f) a)))
     (lambda (g)       
       (f  (lambda (a) ((g g) a))))))) 

(define Y/2                 ; (Y f) = (g g) where
  (lambda (f)             ;         (g g) = (f  (lambda a (apply (g g) a)))
    ((lambda (g) (g g))   ; (Y f) ==        (f  (lambda a (apply (Y f) a)))
     (lambda (g)       
       (f  (lambda (a b) ((g g) a b))))))) 

(define Y/3                 ; (Y f) = (g g) where
  (lambda (f)             ;         (g g) = (f  (lambda a (apply (g g) a)))
    ((lambda (g) (g g))   ; (Y f) ==        (f  (lambda a (apply (Y f) a)))
     (lambda (g)       
       (f  (lambda (a b c) ((g g) a b c))))))) 
 
;; head-recursive factorial
(define fac                ; fac = (Y f) = (f      (lambda a (apply (Y f) a))) 
  (Y/1 (lambda (r)           ;     = (lambda (x) ... (r     (- x 1)) ... )
       (lambda (x)         ;        where   r    = (lambda a (apply (Y f) a))
         (if (< x 2)       ;               (r ... ) == ((Y f) ... )
             1             ;     == (lambda (x) ... (fac  (- x 1)) ... )
             (* x (r (- x 1))))))))
 
;; tail-recursive factorial
(define fac2
  (lambda (x)            
    ((Y/2 (lambda (r)        ;       (Y f) == (f     (lambda a (apply (Y f) a))) 
          (lambda (x acc)  ;          r         == (lambda a (apply (Y f) a))
            (if (< x 2)    ;         (r ... )   == ((Y f) ... )
                acc
                (r (- x 1) (* x acc))))))
     x 1)))
 
; double-recursive Fibonacci
(define fib
  (Y/1 (lambda (f)
       (lambda (x)
         (if (< x 2)
             x
             (+ (f (- x 1)) (f (- x 2))))))))
 
; tail-recursive Fibonacci
(define fib2
  (lambda (x)
    ((Y/3 (lambda (f)
          (lambda (x a b)
            (if (< x 1)
                a
                (f (- x 1) b (+ a b))))))
     x 0 1)))

(begin
 (display (fac 6))
 (newline)

 (display (fac2 6))
 (newline)
 
 (display (fib 10))
 (newline)

 (display (fib 20))
 (newline)
 
 (display (fib2 20))
 (newline))
