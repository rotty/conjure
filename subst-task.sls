;;; subst-task.sls --- Do autoconf-like substitution on files

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (conjure subst-task)
  (export <subst-task>)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (srfi :8 receive)
          (spells pathname)
          (spells filesys)
          (prometheus)
          (conjure utils)
          (conjure file-task))

(define-object <subst-task> (<file-task>)
  (properties `((src       pathname)
                (dest      pathname  ,deduce-dest)
                (escape    string    "#!@")
                (replacer  procedure)))
  
  ((new self resend props)
   (let ((task (resend #f 'new props)))
     (task 'add-value-slot! 'sources (list (task 'prop 'src)))
     (task 'add-value-slot! 'products (list (task 'prop 'dest)))
     task))
  
  ((construct-step self resend project)
   (let ((step (resend #f 'construct-step project)))
     (step 'add-method-slot! 'build
           (lambda (self resend)
             (call-with-input-file (x->namestring (self 'prop 'src))
               (lambda (in-port)
                 (call-with-output-file/atomic (self 'prop 'dest)
                   (lambda (out-port)
                     (subst-port in-port
                                 out-port
                                 (self 'prop 'escape)
                                 (self 'prop 'replacer))))))))
     step)))

(define (deduce-dest task)
  (receive (pathname had-type?)
           (pathname-strip-type (task 'prop 'src) "in")
    (unless had-type?
      (raise-task-error
       'subst
       "no destination given, and source does not have file type 'in'"))
    pathname))

)
