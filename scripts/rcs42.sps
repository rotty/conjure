#!r6rs

(import (rnrs)
        (srfi :8 receive)
        (spells string-substitute)
        (rcs42))

(define (list-ref* lst n default)
  (do ((i 0 (+ i 1))
       (lst lst (cdr lst)))
      ((or (null? lst) (= n i)) (if (null? lst) default (car lst)))))

(define (parse-args args)
  (if (< (length args) 2)
      (bail-out "config file argument required"))
  (values (cadr args) (cddr args)))

(define (make-runner proc . extra-args)
  (lambda (args)
    (receive (cfg args) (parse-args args)
      (apply proc cfg (append extra-args args)))))

(define (println . args)
  (for-each display args)
  (newline))

(define (print-lines entry)
  (if (not (string=? (car entry) "."))
      (println "*** " (car entry)))
  (for-each println (cdr entry)))

(define (make-lister proc output . extra-args)
  (lambda (args)
    (for-each
     (lambda (entry)
       (output entry))
     (receive (cfg args) (parse-args args)
       (apply proc cfg (append extra-args args))))))

(define diff (make-lister config-diff print-lines))
(define inventory (make-lister config-inventory println))
(define dist (make-runner config-dist))
(define push (make-runner build-config 'push))
(define pull (make-runner build-config 'pull))

(define (bail-out msg . args)
  (string-substitute (current-error-port) msg args 'braces)
  (newline (current-error-port))
  (exit 1))

(define (usage)
  (for-each display
            '("usage: rcs42 {diff|inventory|dist|push|pull} CONFIG\n")))

(define (main argv)
  (cond
   ((or (null? argv)
        (null? (cdr argv)))
    (usage)
    (exit 1))
   (else
    ((case (string->symbol (cadr argv))
       ((diff)      diff)
       ((inventory) inventory)
       ((dist)      dist)
       ((push)      push)
       ((pull)      pull)
       (else
        (bail-out "invalid command: {0}" (cadr argv))))
     (cdr argv)))))

(main (command-line))
