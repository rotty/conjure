;;; utils.sls --- Utilities for rcs42

;; Copyright (C) 2008-2011 Andreas Rottmann <a.rottmann@gmx.at>

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
(library (conjure rcs utils)
  (export port->lines
          empty-pathname?
          make-cmd-runner
          make-logged-runner
          make-stdout-runner)
  (import (rnrs)
          (only (srfi :1 lists) unfold)
          (srfi :45 lazy)
          (spells string-utils)
          (spells sysutils)
          (spells pathname)
          (conjure run)
          (conjure utils))

  (define (port->lines port)
    (unfold eof-object?
            values
            (lambda (seed) (get-line port)) (get-line port)))

  (define empty-pathname?
    (let ((empty (make-pathname #f '() #f)))
      (lambda (p)
        (pathname=? p empty))))

  (define (->str x)
    (cond ((string? x)   x)
          ((pathname? x) (x->namestring x))
          ((symbol? x)   (symbol->string x))
          (else
           (assertion-violation '->str "cannot coerce to string" x))))
  
  (define (log-cmd-line cmd-line)
    (display "% ")
    (let loop ((strs (map ->str cmd-line)))
      (unless (null? strs)
        (display (car strs))
        (unless (null? (cdr strs))
          (display " "))
        (loop (cdr strs))))
    (newline)
    (flush-output-port (current-output-port)))

  (define (make-cmd-runner cmd)
    (let ((cmd-path
           (delay (or (find-exec-path cmd)
                      (error 'make-cmd-runner
                             (string-substitute "executable `{0}' not found"
                                                (list cmd)))))))
      (object (<runner>)
        ((run self resend args)
         (resend #f 'run (cons (force cmd-path) args))))))

  (define (make-logged-runner runner)
    (object (runner)
      ((pre-run self resend argv)
       (log-cmd-line argv)
       (resend #f 'pre-run argv))))

  (define (make-stdout-runner runner)
    (object (runner)
      ((term-successful self resend argv status stdout stderr)
       stdout)))
  
  )

;; Local Variables:
;; scheme-indent-styles: ((object 1))
;; End:
