(define (tokenize lst sep acc)
  (if (null? lst)
      (if (null? acc)
          '()
          (cons (reverse acc) '()))
      (if (eq? sep (car lst))
          (cons (reverse acc) (tokenize (cdr lst) sep '()))
          (tokenize (cdr lst) sep (cons (car lst) acc)))))

(for-each print-string (tokenize "Hello,How,Are,You,Today" #\, '()))

