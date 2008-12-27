;; -*- mode: scheme; scheme48-package: rcs42.darcs; -*-

(define (run-darcs command . args)
  (apply run-process #f "darcs" (symbol->string command) args))

(define (run-darcs/log command . args)
  (for-each display `("% darcs " ,command " " ,(string-join args " ") #\newline))
  (flush-output-port (current-output-port))
  (apply run-darcs command args))

(define (port->lines port)
  (unfold eof-object? values (lambda (seed) (read-line port)) (read-line port)))

(define (repos) (call-with-input-file "_darcs/prefs/repos" port->lines))

(define (inventory . args)
  (filter-map (lambda (f)
                (and (not (pathname=? (x->pathname f)
                                      (make-pathname #f '() #f)))
                     f))
              (receive (status sig filenames)
                       (apply run-process/lines #f "darcs" "query" "files" args)
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

(define (config-fold kons nil cfg)
  (define (lose msg . irritants)
    (apply error 'config-fold msg irritants))
  (unless (file-readable? cfg)
    (lose "file not readable" cfg))
  (let ((config (with-input-from-file cfg read))
        (err (lambda (form)
               (lose "config file must be of the form ((dir repo) ...)"
                     form))))
    (if (not (list? config)) (lose "malformed config" config))
    (for-each (lambda (prj)
                (if (not (and (list? prj) (= 2 (length prj)))) (err prj)))
              config)
    (fold (lambda (prj rest)
            (let ((dir (car prj))
                  (repo (cadr prj)))
              (kons dir repo rest)))
          nil
          config)))

(define (config-for-each proc cfg)
  (config-fold (lambda (dir repo rest) (proc dir repo) rest) (unspecific) cfg))

(define (build-config cfg . args)
  (let-optionals* args ((mode 'ask))
    (config-for-each
     (lambda (dir repo)
       (if (file-exists? dir)
           (let ((pull
                  (lambda ()
                    (with-working-directory dir (run-darcs/log 'pull "-a" repo))))
                 (fresh
                  (lambda ()
                    (let ((tmp (temp-name dir)))
                      (rename-file dir tmp)
                      (run-darcs/log 'get repo dir))))
                 (push
                  (lambda ()
                    (with-working-directory dir
                      (run-darcs/log 'push "-a" repo)))))
             (case mode
               ((ask)
                (choose (string-append dir " exists: ")
                        #f
                        `((,(string-append "Update pulling from "
                                           repo)
                           ,pull)
                          ("Get a fresh copy" ,fresh))))
               ((pull) (pull))
               ((fresh) (fresh))
               ((push)  (push))))
           (run-darcs/log 'get repo dir)))
     cfg)))

(define (config-inventory cfg . args)
  (config-fold
   (lambda (dir repo rest)
     (let ((file-list (with-working-directory dir
                        (map (lambda (filename)
                               (if (string=? dir ".")
                                   filename
                                   (string-append dir "/" filename)))
                             (apply inventory args)))))
       (append rest (if (string=? dir ".")
                        file-list
                        (cons dir file-list)))))
   '()
   cfg))

(define (config-whatsnew cfg . args)
  (define (lose msg . irritants)
    (apply error 'config-whatsnew msg irritants))
  (reverse
   (config-fold
    (lambda (dir repo rest)
      (with-working-directory dir
        (receive (status sig result)
                 (call-with-process-output
                     #f (append '("darcs" "whatsnew") args)
                   (lambda (port)
                     (cons (cons dir (port->lines port)) rest)))
          (cond (sig
                 (lose "'darcs whatsnew' killed by signal" sig))
                ((not (memv status '(0 1)))
                 (lose "'darcs whatsnew' exited with unexpected status" status))
                ((= status 1)
                 (cdr result)) ;; throw away output if there were no changes
                (else
                 result)))))
    '()
    cfg)))

(define (config-dist cfg . args)
  (let ((dot-pos (string-index-right cfg #\.))
        (slash-pos (or (string-index-right cfg #\/) -1)))
    (let-optionals* args
        ((name (substring/shared cfg
                                 (+ slash-pos 1)
                                 (or (and dot-pos (> dot-pos slash-pos) dot-pos)
                                     (string-length cfg)))))
      (create-directory name)
      (for-each (lambda (filename)
                  (let ((dst-name (string-append name "/" filename)))
                    (if (file-directory? filename)
                        (create-directory* dst-name)
                        (create-hard-link filename dst-name))))
                (config-inventory cfg))
      (run-process #f "tar" "-czf" (string-append name ".tar.gz") name)
      (run-process #f "rm" "-rf" name))))
