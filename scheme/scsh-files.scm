;; FIXME
(define (get-process-id)
  123456789)

(define (initial-temp-file)
  (let ((tmpdir (lookup-environment-variable "TMPDIR")))
    (string-append
     (if tmpdir
	 tmpdir
	 "/var/tmp")
     "/"
     (number->string (get-process-id))
     "~a")))

(define *temp-file-template*
  (make-parameter (initial-temp-file)))

(define (temp-file-iterate maker . maybe-template)
  (let ((template (if (null? maybe-template) (*temp-file-template*)
                      (car maybe-template))))
    (let loop ((i 0))
      (if (> i 1000) (error "Can't create temp-file")
	  (let ((fname (format #f template (number->string i))))
	    (receive retvals (guard
                              (c ((i/o-error? c) #f))
                              (maker fname))
	      (if (car retvals) (apply values retvals)
		  (loop (+ i 1)))))))))
