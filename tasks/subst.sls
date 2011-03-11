;;; subst-task.sls --- Do autoconf-like substitution on files

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (conjure tasks subst)
  (export <subst-task>)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (srfi :8 receive)
          (spells pathname)
          (spells filesys)
          (wak prometheus)
          (conjure utils)
          (conjure base))

(define (deduce-dest task)
  (receive (pathname had-type?)
           (pathname-strip-type (task 'prop 'src) "in")
    (unless had-type?
      (raise-task-error
       'subst
       "no destination given, and source does not have file type 'in'"))
    pathname))

(define-object <subst-step> (<file-step>)
  ((build self resend)
   (call-with-input-file (x->namestring (self 'prop 'src))
     (lambda (in-port)
       (call-with-output-file/atomic (self 'prop 'dest)
         (lambda (out-port)
           (subst-port in-port
                       out-port
                       (self 'prop 'escape)
                       (self 'prop 'replacer))))))
   (resend #f 'build)))

(define-object <subst-task> (<file-task>)
  (properties `((src       pathname)
                (dest      pathname  ,deduce-dest)
                (escape    string    "#!@")
                (replacer  procedure)))
  (step-prototype <subst-step>)
  
  ((new self resend name args props)
   (let ((task (resend #f 'new name args props)))
     (task 'add-value-slot! 'sources (list (task 'prop 'src)))
     (task 'add-value-slot! 'products (list (task 'prop 'dest)))
     task)))

)
