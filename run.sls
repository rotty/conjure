;;; run.sls --- Convience procedures to run programs

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

(library (conjure run)
  (export <runner>
          runner-error?
          runner-error-argv)
  (import (rnrs)
          (srfi :8 receive)
          (only (srfi :13 strings) string-join)
          (spells process)
          (spells ports)
          (spells pathname)
          (only (spells filesys) working-directory)
          (spells string-utils)
          (spells misc)
          (wak prometheus)
          (conjure utils))

(define-condition-type &runner-error &error
  make-runner-error runner-error?
  (argv runner-error-argv))

(define (runner-error argv message args)
  (raise (condition (make-who-condition '<runner>)
                    (make-runner-error argv)
                    (make-message-condition
                     (string-substitute message
                                        (cons* `(argv . ,(argv->string argv))
                                               args)
                                        'braces)))))

(define-object <runner> (*the-root-object*)
  (env #f)
  (success-codes '(0))
  (stdout #t)
  (stderr #t)

  ((run self resend argv)
   (let ((stdout (self 'stdout))
         (stderr (self 'stderr)))
     (self 'pre-run argv)
     (receive
         (status sig stdout stderr)
         (cond ((and (eqv? stdout #t)
                     (eqv? stderr #t))
                (receive (status sig)
                         (apply run-process (self 'env) argv)
                  (values status sig #f #f)))
               ((eqv? stderr #t)
                (receive (status sig stdout)
                         (apply
                          (case stdout
                            ((sexps)  run-process/sexps)
                            ((lines)  run-process/lines)
                            ((string) run-process/string)
                            (else
                             (error '<runner>
                                    "invalid capture mode for stdout" stdout)))
                          (self 'env) argv)
                  (values status sig stdout #f)))
               (else
                (error '<runner>
                       "capturing stderr not yet supported"
                       stdout stderr)))
       (cond (sig
              (self 'term-signal argv sig))
             ((memv status (self 'success-codes))
              (self 'term-successful argv status stdout stderr))
             (else
              (self 'term-unsuccessful argv status stdout stderr))))))

  ((pre-run self resend argv)
   (unspecific))
  
  ((term-successful self resend argv status stdout stderr)
   (unspecific))
  ((term-unsuccessful self resend argv status stdout stderr)
   (runner-error argv
                 (string-append
                  "child process ({argv}) terminated with unexpected"
                  " status {status} (not in {success-codes})\n"
                  "working directory: {cwd}")
                 `((status . ,status)
                   (success-codes . ,(string-join (map number->string
                                                       (self 'success-codes))
                                                  ", "))
                   (cwd . ,(x->namestring (working-directory))))))
  ((term-signal self resend argv sig)
   (runner-error argv
                 "child process ({argv}) terminated by signal {sig}"
                 `((sig . ,sig)))))

(define (argv->string argv)
  (define (->str x)
    (cond ((string? x) x)
          (else
           (x->namestring x))))
  (string-join (map ->str argv) " "))

)

;; Local
