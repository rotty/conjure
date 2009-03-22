;;; task-lib.sls --- Conjure task library

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

(library (conjure task-lib)
  (export find-task-prototype
          register-task-prototype
          register-builtin-tasks)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs mutable-pairs)
          (spells define-values)
          (spells alist)
          (conjure file-task))

(define-values (find-task-prototype register-task-prototype)
  (let ((prototypes '()))
    (values
     (lambda (name)
       (assq-ref prototypes name))
     (lambda (name prototype)
       (cond ((assq name prototypes)
              => (lambda (entry)
                   (set-cdr! entry prototype)))
             (else
              (set! prototypes (cons (cons name prototype) prototypes))))))))

(define register-builtin-tasks
  (let ((registered? #f))
    (lambda ()
      (unless registered?
        (register-task-prototype 'file <file-task>)
        (set! registered? #t)))))

)
