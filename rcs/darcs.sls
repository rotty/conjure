#!r6rs

(library (conjure rcs darcs)
  (export darcs)
  
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs control)
          (rnrs lists)
          (rnrs io simple)
          (rnrs io ports)
          (only (srfi :1 lists) filter-map)
          (srfi :8 receive)
          (srfi :13 strings)
          (spells pathname)
          (spells filesys)
          (spells process)
          (spells sysutils)
          (spells delimited-readers)
          (spells misc)
          (spells opt-args)
          (spells tracing)
          (spells include)
          (spells operations)
          (xitomatl irregex)
          (conjure rcs utils)
          (conjure rcs files)
          (conjure rcs prompt)
          (conjure rcs operations))

(define darcs-command
  (or (find-exec-path "darcs")
      (error 'darcs-command "darcs executable not found")))

(define (run-darcs command . args)
  (define (lose msg . irritants)
    (apply error
           (string-append "darcs command '" (symbol->string command) "' " msg)
           irritants))
  (receive (status sig)
           (apply run-process #f darcs-command (symbol->string command) args)
    (cond (sig
           (lose "killed by signal" sig))
          ((not (= status 0))
           (lose "exited with non-zero status" status)))))

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
                       (run-process/lines #f darcs-command "query" "files")
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


(define (diff)
  (define (lose msg . irritants)
    (apply error 'darcs-diff msg irritants))
  (receive (status sig lines)
           (run-process/lines #f darcs-command "diff" "-u")
    (cond (sig
           (lose "'darcs diff' killed by signal" sig))
          ((not (memv status '(0 1)))
           (lose "'darcs diff' exited with unexpected status" status))
          ((= status 1)
           (cdr lines)) ;; throw away output if there were no changes
          (else
           lines))))

(define darcs
  (object #f
    ((rcs/pull self repo branch)
     (run-darcs/log 'pull "-a" repo))
    ((rcs/push self repo branch)
     (run-darcs/log 'push "-a" repo))
    ((rcs/inventory self)
     (inventory))
    ((rcs/diff self)
     (diff))
    ((rcs/get self repo dir)
     (run-darcs/log 'get repo dir))))
  
)
