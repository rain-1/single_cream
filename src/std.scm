;;; queue

(define (empty-queue) (list->vector (list 'queue '() #f)))

(define (queue:top q) (vector-ref q 1))
(define (queue:bot q) (vector-ref q 2))
(define (queue:top! q v) (vector-set! q 1 v))
(define (queue:bot! q v) (vector-set! q 2 v))

(define (queue-push! q v)
  (if (queue:bot q)
      (begin (set-cdr! (queue:bot q) (list v))
	     (queue:bot! q (cdr (queue:bot q))))
      (begin (queue:top! q (list v))
	     (queue:bot! q (queue:top q)))))

(define (queue-pop! q)
  (let ((top (queue:top q)))
    (if (null? top)
	(error 'queue-pop! 0 0)
	(begin
	  (queue:top! q (cdr top))
	  (when (null? (cdr top))
	    (queue:bot! q #f))
	  (car top)))))

(define (queue->list q)
  (copy-list (queue:top q)))


;;; seq

(define (elt? exp)
  (and (pair? exp)
       (eq? 'elt (car exp))
       (pair? (cdr exp))
       (null? (cddr exp))))
(define (elt-get-elt exp) (cadr exp))
(define (cat? exp) (and (pair? exp) (eq? 'cat (car exp))))
(define (cat-get-seqs exp) (cdr exp))

(define (seq->dlist seq tail)
  (cond ((elt? seq) (cons (elt-get-elt seq) tail))
	((cat? seq) (fold seq->dlist tail (cat-get-seqs seq)))
	(else (error 'seq->dlist "?" seq))))
(define (seq->list seq) (seq->dlist seq '()))

(define (seq-length^ seq tail)
  (cond ((elt? seq) (+ 1 tail))
	((cat? seq) (fold seq-length^ tail (cat-get-seqs seq)))
	(else (error 'seq-length^ "?" seq))))
(define (seq-length seq) (seq-length^ seq 0))


;;; stack

(define (empty-stack) (box '()))

(define (make-stack lst) (box lst))

(define (stack-top s)
  (let ((stk (unbox s)))
    (if (null? stk)
	(error 'stack-top/null 0 0)
	(car stk))))

(define (stack-pop! s)
  (let ((stk (unbox s)))
    (if (null? stk)
	(error 'stack-pop/null 0 0)
	(begin
	  (set-box! s (cdr stk))
	  (car stk)))))

(define (stack-push! s v) (set-box! s (cons v (unbox s))))

(define (stack-get s) (unbox s))


;;; numbers

;; char->integer

(define (digit->number d) (- (char->integer d) (char->integer #\0)))
(define (string->number s)
  (let ((l (string-length s)))
    (let loop ((n 0) (i 0))
      (if (= i l)
	  n
	  (let ((digit (string-ref s i)))
	    (loop (+ (* 10 n) (digit->number digit)) (+ i 1)))))))
