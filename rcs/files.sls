#!r6rs

(library (conjure rcs files)
  (export temp-name
          create-temp-directory)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs io ports)
          (srfi :8 receive)
          (srfi :39 parameters)
          (spells format)
          (spells delimited-readers)
          (spells filesys)
          (spells sysutils)
          (spells include))

(define (create-temp-directory . prefix)
  (let ((prefix (or (and (not (null? prefix)) (car prefix))
                    "/var/tmp/")))
    (temp-file-iterate (lambda (dir) (create-directory dir) dir)
                       (string-append prefix ".~a"))))

;; Note that this procedure is *not* safe -- there's a race
;; condition.
(define (temp-name . prefix)
  (let ((prefix (or (and (not (null? prefix)) (car prefix))
                    "/var/tmp/")))
    (receive (ign filename) (temp-file-iterate
                              (lambda (f)
                                (values (not (file-exists? f)) f))
                              (string-append prefix ".~a"))
        filename)))

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
      (if (> i 1000) (error 'temp-file-iterate "Can't create temp-file")
	  (let ((fname (format #f template (number->string i))))
	    (receive retvals (guard
                              (c ((i/o-error? c) #f))
                              (maker fname))
	      (if (car retvals) (apply values retvals)
		  (loop (+ i 1)))))))))

)
