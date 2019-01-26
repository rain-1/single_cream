(define (scan-1 code place lbls)
  ;; scan-1 is the first pass of the assembler
  ;;
  ;; it flattens the list of lists (instructions are short lists)
  ;; into one long list.
  ;;
  ;; it also looks for labels, erasing them from the output but
  ;; noting their positions
  ;;
  ;; (label <lbl>)
  ;;
  (if (null? code)
      #t
      (let ((inst (car code))
	    (rest (cdr code)))
	(if (label? inst)
	    (begin
	      (stack-push! lbls (cons (label-get-name inst) place))
	      (scan-1 rest
		      place
		      lbls))
	    (scan-1 rest
		    (+ place (length inst))
		    lbls)))))

(define (resolve-label place lbl lbls)
  (cond ((assoc lbl (stack-get lbls)) =>
	 (lambda (entry)
	   (- (cdr entry) place)))
	(else (error 'resolve-label "couldn't find label" 0))))

(define (mad-helper inst place lbls)
 (cond ((branch? inst)
        `(branch ,(resolve-label place (branch-get-label inst) lbls)))
 	((jump? inst)
         `(jump ,(resolve-label place (jump-get-label inst) lbls)))
	((stackframe? inst)
	 `(stackframe ,(resolve-label place (stackframe-get-label inst) lbls)))
        ((allocate-closure? inst)
	 `(allocate-closure
		     ,(allocate-closure-get-size inst)
		     ,(resolve-label place (allocate-closure-get-label inst) lbls)))
	((information? inst)
	 `(information ,(resolve-label place (information-get-label-1 inst) lbls)
		       ,(resolve-label place (information-get-label-2 inst) lbls)
                                         ,(information-get-info inst)))
        (else inst)))

(define (scan-2 code place lbls)
  ;; scan-2 is the second pass
  ;;
  ;; it looks for certain special forms that reference
  ;; labels, and replaces the label reference with a
  ;; relative offset
  ;;
  ;; (branch <lbl>)
  ;; (jump <lbl>)
  ;; (stackframe <lbl>)
  ;; (allocate-closure <size> <lbl>)
  ;;
  (if (null? code)
      '()
      (let ((inst (car code))
	    (rest (cdr code)))
	(if (label? inst)
	    (scan-2 rest place lbls)
	    (let ((place (+ place (length inst))))
	      (append (mad-helper inst place lbls)
		      (scan-2 rest
			      place
			      lbls)))))))

(define (assemble code)
  ;; lbls is an assoc list about label definitions
  ;; the entries are:
  ;;  (<label-name> . <label-position>)
  ;;
  ;; lbl-refs is a queue about label uses
  ;; the entries are:
  ;;  (<label-name> . <position>)
  ;;
  (let* ((lbls (empty-stack)))
    (scan-1 code 0 lbls)
    (scan-2 code 0 lbls)))
