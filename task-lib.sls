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
  (export register-builtin-tasks)
  (import (rnrs base)
          (rnrs control)
          (conjure base)
          (conjure cc)
          (conjure tasks subst)
          (conjure tasks configure))


(define register-builtin-tasks
  (let ((registered? #f))
    (lambda ()
      (unless registered?
        (register-task-prototype 'ordinary <ordinary-task>)
        (register-task-prototype 'file <file-task>)
        (register-task-prototype 'subst <subst-task>)
        (register-task-prototype 'configure <configure-task>)
        (register-task-prototype 'cc-conf <cc-conf-task>)
        (set! registered? #t)))))

)
