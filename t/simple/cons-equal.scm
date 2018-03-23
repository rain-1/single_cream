(equal? 1 1)
(equal? 1 2)

(equal? (cons 1 2) '(1 . 2))
(equal? '(1 . 2) (cons 1 2))

(equal? (cons 1 2) '(1 . 3))
(equal? (cons 2 1) '(1 . 2))

(equal? (cons 1 (cons 2 (cons 3 '())))
        '(1 2 3))
(equal? (cons 1 (cons 2 (cons 3 (cons 4 '()))))
        '(1 2 3 4))
(equal? (cons 1 (cons 2 (cons 3 4)))
        '(1 2 3 4))
