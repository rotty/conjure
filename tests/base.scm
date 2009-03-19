;;; base.scm --- Unit tests for (conjure base)

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

(define-test-suite base-tests
  "Base library")

(define (deduce-dest t)
  (pathname-add-type (t 'prop 'src) "out"))

(define-object <test-task> (<task>)
  (properties `((src    pathname)
                (dest   pathname ,deduce-dest)
                (escape string "@"))))

(define-test-case base-tests task-props ()
  (let ((t (<test-task> 'new '((src . "a")))))
    (test-compare pathname=? (x->pathname '(() "a")) (t 'prop 'src))
    (test-compare pathname=? (x->pathname '(() ("a" "out"))) (t 'prop 'dest))
    (test-equal "@" (t 'prop 'escape))))

(run-test-suite base-tests)
