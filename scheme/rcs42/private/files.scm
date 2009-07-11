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
