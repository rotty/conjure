(define (file->string-list filename)
  (call-with-input-file filename
    (lambda (port)
      (unfold eof-object? values (lambda (seed) (read-line port)) (read-line port)))))

(define (create-temp-directory . prefix)
  (let ((prefix (or (and (not (null? prefix)) (car prefix))
                    "/var/tmp/")))
    (temp-file-iterate (lambda (dir) (make-directory dir) dir)
                       (string-append prefix ".~a"))))

(define (temp-name . prefix)
  (let ((prefix (or (and (not (null? prefix)) (car prefix))
                    "/var/tmp/")))
    (receive (ign filename) (temp-file-iterate
                              (lambda (f)
                                (values (not (accessible? f (access-mode exists))) f))
                              (string-append prefix ".~a"))
        filename)))

(define-syntax with-cwd
  (syntax-rules ()
    ((with-cwd wd body ...)
     (let ((old-wd (working-directory)))
       (dynamic-wind
           (lambda () (set-working-directory! wd))
           (lambda () body ...)
           (lambda () (set-working-directory! old-wd)))))))


(define (normalize-filename filename)
  (if (string-prefix? "./" filename)
      (substring/shared filename 2 (string-length filename))
      filename))
