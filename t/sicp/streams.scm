(defmacro cons-stream
  (lambda (exps)
    (let ((car (cadr exps))
	  (cdr (caddr exps)))
      `(cons ,car (lambda () ,cdr)))))

(define modulo remainder)

(define (divisible? n d)
  (= 0 (modulo n d)))

(define the-empty-stream '())
(define (stream-null? e) (null? e))

(define (force* delayed-object)
  (delayed-object))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force* (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (take strm n)
  (if (= n 0)
      '()
      (cons (stream-car strm)
            (take (stream-cdr strm) (- n 1)))))

(print (take fibs 20))

(print (take primes 100))
