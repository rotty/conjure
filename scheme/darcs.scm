;; -*- mode: scheme; scheme48-package: rcs42.darcs; -*-

(define (port->lines port)
  (unfold eof-object? values (lambda (seed) (read-line port)) (read-line port)))

(define (run-process/lines prog . args)
  (call-with-process-output (cons prog args) port->lines))

(define (run-darcs command . args)
  (apply run-process #f "darcs" (symbol->string command) args))

(define (run-darcs/log command . args)
  (for-each display `("% darcs " ,command " " ,(string-join args " ") #\newline))
  (force-output (current-output-port))
  (apply run-darcs command args))

(define (repos) (file->string-list "_darcs/prefs/repos"))

(define (inventory . args)
  (filter-map (lambda (f) (and (not (string=? f "."))
                               (normalize-filename f)))
              (apply run-process/lines "darcs" "query" "manifest" args)))

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

(define http-rx (make-regexp "^http://"))

(define (push)
  (let* ((repos (remove (lambda (r) (regexp-match http-rx r 0 #f #t #t)) (repos)))
         (len (length repos)))
    (if (> len 0)
        (choose "Push to repository:"
                (> len 1)
                (map (lambda (repo) `(,repo ,(lambda () (run-darcs 'push repo))))
                     repos)))))

(define (config-fold kons nil cfg)
  (or (accessible? cfg (access-mode read))
      (error "file not readable" cfg))
  (let ((config (with-input-from-file cfg read))
        (err (lambda (form)
               (error "config file must be of the form ((dir repo) ...)"
                      form))))
    (if (not (list? config)) (error config))
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
       (if (accessible? dir (access-mode exists))
           (let ((pull
                  (lambda ()
                    (with-cwd dir (run-darcs/log 'pull "-a" repo))))
                 (fresh
                  (lambda ()
                    (let ((tmp (temp-name dir)))
                      (rename dir tmp)
                      (run-darcs/log 'get repo dir))))
                 (push
                  (lambda ()
                    (with-cwd dir ( run-darcs/log 'push "-a" repo)))))
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
     (let ((file-list (with-cwd dir
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
  (reverse (config-fold
            (lambda (dir repo rest)
              (with-cwd dir
                (call-with-process-output (append '("darcs" "whatsnew") args)
                    (lambda (port)
                      (cons (cons dir (port->lines port)) rest))
                  (lambda (status sig result)
                    (cond (sig
                           (error "'darcs whatsnew' killed by signal" sig))
                          ((not (memv status '(0 1)))
                           (error "'darcs whatsnew' exited with unexpected status" status))
                          ((= status 1)
                           (cdr result)) ;; throw away output if there were no changes
                          (else
                           result))))))
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
      (make-directory name (file-mode owner))
      (for-each (lambda (filename)
                  (let ((info (get-file-info filename))
                        (dst-name (string-append name "/" filename)))
                    (if (eq? (file-info-type info) (file-type directory))
                        (make-directory dst-name (file-info-mode info))
                        (link filename dst-name))))
                (config-inventory cfg "--directories"))
      (run-process #f "tar" "-czf" (string-append name ".tar.gz") name)
      (run-process #f "rm" "-rf" name))))
