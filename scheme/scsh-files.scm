(define (initial-temp-file)
  (let ((tmpdir (lookup-environment-variable "TMPDIR")))
    (string-append
     (if tmpdir
	 tmpdir
	 "/var/tmp")
     "/"
     (number->string (get-process-id))
     "~a")))

(define *temp-file-template* (make-fluid 'not-initialized-temp-file-template))

(define temp-file-reinitializer 
  (make-reinitializer 
   (lambda ()
     (set-fluid! *temp-file-template* (initial-temp-file)))))

(define (temp-file-iterate maker . maybe-template)
  (let ((template (if (null? maybe-template) (fluid *temp-file-template*)
                      (car maybe-template))))
    (let loop ((i 0))
      (if (> i 1000) (error "Can't create temp-file")
	  (let ((fname (format #f template (number->string i))))
	    (receive retvals (guard
                              (c ((i/o-error? c) #f))
                              (maker fname))
	      (if (car retvals) (apply values retvals)
		  (loop (+ i 1)))))))))
