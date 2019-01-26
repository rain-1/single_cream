;;;;;;;
;; tokenizer

(define (tokenize queue port)
  (let ((ch (read-char port)))
    (if (eof-object? ch)
	'()
	(case ch
	  ((#\space #\tab #\newline) (tokenize queue port))
	  ((#\;) (state:comment queue port))
	  ((#\() (state:open queue port))
	  ((#\)) (state:close queue port))
	  ((#\') (state:quote queue port))
	  ((#\`) (state:quasiquote queue port))
	  ((#\,) (state:unquote queue port))
	  ((#\.) (state:dot queue port))
	  ((#\#) (state:hash queue port))
	  ((#\") (state:string queue port))
	  (else (state:item queue port (cons ch '())))))))

(define (numeric-char? ch)
  (case ch
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) #t)
    (else #f)))

(define (make-symbol-token l)
  (let ((r (reverse l)))
    (cond ((and (eq? #\- (car r)) (not (null? (cdr r))) (numeric-char? (cadr r)))
	   (- 0 (string->number (list->string r))))
	  ((numeric-char? (car r))
	   (string->number (list->string r)))
	  (else (string->symbol (list->string r))))))

(define (state:item queue port acc)
  (let ((ch (peek-char port)))
    (if (eof-object? ch)
	(queue-push! queue (make-symbol-token acc))
	(case ch
	  ((#\space #\tab #\newline #\;
	    #\( #\) #\' #\` #\, #\. #\#)
	   (queue-push! queue (make-symbol-token acc))
	   (tokenize queue port))
	  (else (state:item queue port (cons (read-char port) acc)))))))

(define (state:comment queue port)
  (let ((ch (read-char port)))
    (if (eof-object? ch)
	'()
	(case ch
	  ((#\newline) (tokenize queue port))
	  (else (state:comment queue port))))))

(define special:open (gensym "open"))
(define (state:open queue port)
  (queue-push! queue special:open)
  (tokenize queue port))

(define special:close (gensym "close"))
(define (state:close queue port)
  (queue-push! queue special:close)
  (tokenize queue port))

(define (state:string queue port)
  (queue-push! queue (list->string (string:char port)))
  (tokenize queue port))

(define (string:char port)
  (let ((c (read-char port)))
    (if (eof-object? c)
	(error 'unescape:char "string ended too soon" 0)
	(case c
	  ((#\\) (string:escape port))
	  ((#\") '())
	  (else (cons c (string:char port)))))))

(define (string:escape port)
  (let ((c (read-char port)))
    (if (eof-object? c)
	(error 'unescape:esc "string ended too soon" 0)
	(cons c (string:char port)))))

(define special:quote (gensym "quote"))
(define (state:quote queue port)
  (queue-push! queue special:quote)
  (tokenize queue port))

(define special:quasiquote (gensym "quasiquote"))
(define (state:quasiquote queue port)
  (queue-push! queue special:quasiquote)
  (tokenize queue port))

(define special:unquote (gensym "unquote"))
(define (state:unquote queue port)
  (queue-push! queue special:unquote)
  (tokenize queue port))

(define special:dot (gensym "dot"))
(define (state:dot queue port)
  (queue-push! queue special:dot)
  (tokenize queue port))

(define (state:hash queue port)
  (let ((ch (read-char port)))
    (if (eof-object? ch)
	'()
	(case ch
	  ((#\t) (state:true queue port))
	  ((#\f) (state:false queue port))
	  ((#\\) (state:char queue port))
	  ;((#\x) (state:hex port))
	  (else (state:comment queue port))))))

(define (state:true queue port)
  (queue-push! queue #t)
  (tokenize queue port))

(define (state:false queue port)
  (queue-push! queue #f)
  (tokenize queue port))

(define (state:char queue port)
  (let* ((ch (read-char port))
  	 (nxt (peek-char port)))
    (case nxt
      ((#\space #\tab #\newline #\;
	#\( #\) #\' #\` #\, #\. #\#)
       (queue-push! queue ch)
       (tokenize queue port))
      (else
       (state:long-char queue port (cons ch '()))))))

(define (state:long-char queue port acc)
  (let ((ch (peek-char port)))
    (case ch
      ((#\space #\tab #\newline #\;
	#\( #\) #\' #\` #\, #\. #\#)
       (case (make-symbol-token acc)
	 ((space)
	  (queue-push! queue #\space)
	  (tokenize queue port))
	 ((tab)
	  (queue-push! queue #\tab)
	  (tokenize queue port))
	 ((newline)
	  (queue-push! queue #\newline)
	  (tokenize queue port))
	 (else (begin (print (make-symbol-token acc))
		      (error 'unknown-long-char "unknown long char" acc)))))
      (else
       (state:long-char queue port (cons (read-char port) acc))))))

(define (state:hex port)
  ;;
  0
  )

;;;;;;;
;; parser

(define (parse tokens)
  (if (null? (queue:top tokens))
      '()
      (let ((elt (parse-1 tokens)))
	(cons elt (parse tokens)))))

(define (parse-1 tokens)
  (if (null? (queue:top tokens))
      (error 'parse-eof "parse eof" 0)
      (let ((tok (queue-pop! tokens)))
	(cond ((equal? tok special:open) (parse:open tokens))
	      ((equal? tok special:close) (error 'parse-early-close "parse early tokens" 0))
	      ((equal? tok special:quote) (list 'quote (parse-1 tokens)))
	      ((equal? tok special:quasiquote) (list 'quasiquote (parse-1 tokens)))
	      ((equal? tok special:unquote) (list 'unquote (parse-1 tokens)))
	      (else tok)))))

(define (queue-peek q)
  (car (queue:top q)))

(define (parse:open tokens)
  (if (null? (queue:top tokens))
      (error 'parse:open-unexpected-eof "unexpected eof" 0)
      (let ((tok (queue-peek tokens)))
	(cond ((equal? tok special:close)
	       (queue-pop! tokens)
	       '())
	      ((equal? tok special:dot)
	       (queue-pop! tokens)
	       (let ((r (parse-1 tokens)))
                 (let ((next-tok (queue-pop! tokens)))
  		   (unless (equal? special:close next-tok)
                     (print `(bad token was ,next-tok))
                     (print `(parse-1 was ,r))
		     (error 'didnt-close-after-dot "didnt close after dot" next-tok)))
		 r))
	      (else (let ((elt (parse-1 tokens)))
		      (cons elt (parse:open tokens))))))))


;;;;;;;;;;

(define (read-port port)
  (let ((q (empty-queue)))
    (tokenize q port)
    (parse q)))

(define (read-file filename)
  (with-input-file filename
    (lambda (port)
      (read-port port))))

;(read-file "./t/tarot/parser.scm")
