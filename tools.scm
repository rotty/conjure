(define (list-ref* lst n default)
  (do ((i 0 (+ i 1))
       (lst lst (cdr lst)))
      ((or (null? lst) (= n i)) (if (null? lst) default (car lst)))))

(define (parse-args args)
  (if (< (length args) 2)
      (error "config file argument required"))
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

(define whatsnew (make-lister config-whatsnew print-lines))
(define inventory (make-lister config-inventory println))
(define dist (make-runner config-dist))
(define push (make-runner build-config 'push))
(define pull (make-runner build-config 'pull))
