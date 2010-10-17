;;; cmd-line.sls --- Command-line handling

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (conjure cmd-line)
  (export command-line-ui)
  (import (rnrs)
          (srfi :8 receive)
          (only (srfi :13) string-prefix?)
          (spells alist)
          (spells args-fold)
          (wak foof-loop)
          (wak fmt))

(define (parse-command-line command-line)
  (parse-operands command-line
                  (lambda (bracketed rest)
                    (error 'parse-command-line
                           "unexpected closing bracket" command-line))))

(define (parse-operands operands close-cont)
  (define (lose message . irritants)
    (apply error 'parse-args message irritants))
  (define (extend-result result last-arg)
    (if last-arg (cons last-arg result) result))
  (loop continue ((for arg arg-rest (in-list operands))
                  (with last-arg #f arg)
                  (with result '()))
    => (reverse (extend-result result last-arg))
    (cond ((string=? "[" arg)
           (unless last-arg
             (lose "unexpected opening bracket" operands))
           (parse-operands (cdr arg-rest)
                           (lambda (bracketed rest)
                             (continue (=> result (cons (cons last-arg bracketed)
                                                        result))
                                       (=> last-arg #f)
                                       (=> arg-rest rest)))))
          ((string=? "]" arg)
           (close-cont (extend-result result last-arg) (cdr arg-rest)))
          (else
           (continue (=> result (extend-result result last-arg)))))))

(define (option-flag-processor flag-name)
  (lambda (option name arg options operands)
    (values (cons (cons flag-name #t) options)
            operands)))

(define (process-operand operand options operands)
  (values options (cons operand operands)))

(define (unrecognized-option option name arg options operands)
  (raise-ui-error (cat "unrecognized option: " name)))

(define-condition-type &ui-error &error
  make-ui-error ui-error?)

(define (raise-ui-error message)
  (raise (condition (make-ui-error)
                    (make-message-condition (fmt #f message)))))

(define ui-options
  (list (option '(#\c "clean") #f (option-flag-processor 'clean))))

(define (command-line-ui project args)
  (receive (options targets)
           (args-fold (parse-command-line args)
                      ui-options
                      unrecognized-option
                      process-operand
                      '()
                      '())
    (let ((action (cond ((assq-ref options 'clean)
                         'clean)
                        (else
                         'build-rec))))
      (project 'invoke action (reverse targets)))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
