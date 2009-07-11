#!r6rs

(library (conjure rcs prompt)
  (export y-or-n choose)
  (import (rnrs)
          (spells include))

(define (y-or-n prompt)
  (member (begin (display prompt) (read)) '(y yes ok)))

(define (choose title multiple? spec)
  (call-with-current-continuation
   (lambda (k)
     (let ((real-spec
            (let loop ((in-spec spec) (n 1) (new-spec '()))
              (cond ((null? in-spec)
                     (reverse (cons `(0 Done ,(lambda () (k 'done))) new-spec)))
                    (else (loop (cdr in-spec)
                                (+ 1 n)
                                (cons (cons n (car in-spec)) new-spec)))))))
       (let loop ((opt #f))
         (let ((sp (assoc opt real-spec)))
           (if sp (begin ((caddr sp)) (if (not multiple?) (k 'done))))
           (display title) (newline)
           (for-each (lambda (opt desc)
                       (display opt) (display ": ") (display desc) (newline))
                     (map car real-spec)
                     (map cadr real-spec))
           (display "Option: ")
           (loop (read))))))))

)
