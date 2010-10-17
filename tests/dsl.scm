;;; dsl.scm --- Unit tests for the Conjure DSL

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

(define-test-suite dsl-tests
  "Domain specific language")

(define-test-case dsl-tests basics ()
  (let ((result '()))
    (test-equal '(bar qux foo)
      (let ()
        (define (result-proc self)
          (set! result (cons (self 'name) result)))
        (define-project test-project ()
          (task foo (ordinary
                      (depends 'bar 'qux)
                      (proc result-proc)))
          (task bar (ordinary
                     (proc result-proc)))
          (task qux (ordinary
                     (depends 'bar)
                     (proc result-proc))))
        (test-project 'build-rec)
        (reverse result)))))

(register-builtin-tasks)

(run-test-suite dsl-tests)
