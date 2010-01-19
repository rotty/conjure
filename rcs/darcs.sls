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
          (spells irregex)
          (rename (only (conjure utils) object)
                  (object obj))
          (conjure rcs utils)
          (conjure rcs prompt)
          (conjure rcs operations))

(define <darcs-runner> (make-cmd-runner "darcs"))
(define <darcs-runner/log> (make-logged-runner <darcs-runner>))
(define <darcs-runner/stdout> (make-stdout-runner <darcs-runner>))

(define (run-darcs command . args)
  (<darcs-runner> 'run (cons (symbol->string command) args)))

(define (run-darcs/log command . args)
  (<darcs-runner/log> 'run (cons (symbol->string command) args)))

(define (repos) (call-with-input-file "_darcs/prefs/repos" port->lines))

(define inventory
  (let ((runner (obj (<darcs-runner/stdout>) (stdout 'lines))))
    (lambda ()
      (filter-map (lambda (f)
                    (and (not (pathname=? (x->pathname f)
                                          (make-pathname #f '() #f)))
                         f))
                  (runner 'run '("query" "files"))))))

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

(define diff
  (let ((runner (obj (<darcs-runner>)
                  (stdout 'lines)
                  ((term-successful self resend argv status stdout stderr)
                   (case status
                     ((0) stdout)
                     ((1)  ; throw away output if there were no changes
                      (cdr stdout)))))))
    (lambda ()
      (runner 'run '("diff" "-u")))))

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

;; Local Variables:
;; scheme-indent-styles: ((object 1) (obj 1))
;; End:
