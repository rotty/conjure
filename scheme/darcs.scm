;; -*- mode: scheme; scheme48-package: rcs42.darcs; -*-

(define (run-darcs command . args)
  (apply run-process #f "darcs" (symbol->string command) args))

(define (run-darcs/log command . args)
  (for-each display `("% darcs " ,command " " ,(string-join args " ") #\newline))
  (flush-output-port (current-output-port))
  (apply run-darcs command args))

(define (repos) (call-with-input-file "_darcs/prefs/repos" port->lines))

(define (inventory)
  (filter-map (lambda (f)
                (and (not (pathname=? (x->pathname f)
                                      (make-pathname #f '() #f)))
                     f))
              (receive (status sig filenames)
                       (run-process/lines #f "darcs" "query" "files")
                (if (= 0 status)
                    filenames
                    '()))))

(define (pull)
  (let* ((repos (repos))
         (len (length repos)))
    (if (> len 0)
        (choose "Pull from repository:"
                (> len 1)
                (map (lambda (repo)
                       `(,repo ,(lambda ()
                                  (run-darcs 'pull repo))))
                     repos)))))

(define http-rx (irregex '(: bos "http://")))

(define (push)
  (let* ((repos (remove (lambda (r) (irregex-search http-rx r)) (repos)))
         (len (length repos)))
    (if (> len 0)
        (choose "Push to repository:"
                (> len 1)
                (map (lambda (repo) `(,repo ,(lambda () (run-darcs 'push repo))))
                     repos)))))

(define darcs
  (object #f
    ((rcs/pull self)      (pull))
    ((rcs/push self)      (push))
    ((rcs/inventory self) (inventory))))
