;;; base.scm --- Unit tests for (conjure base)

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

(import (except (rnrs) file-exists? delete-file)
        (spells pathname)
        (spells tracing) ;debug
        (spells filesys)
        (wak trc-testing)
        (wak prometheus)
        (conjure utils)
        (conjure base))

(define test-dir (->pathname '((",base-test.tmp"))))

(define (test-filename pathname)
  (merge-pathnames pathname test-dir))

(define (assert-clear-stage)
  (when (file-exists? test-dir)
    (test-failure "working stage not clear" test-dir)))

(define-test-suite base-tests
  "Base library")


(define (deduce-dest t)
  (pathname-add-type (t 'prop 'src) "out"))

(define-object <test-task> (<task>)
  (arguments '(dest))
  (properties `((src    pathname)
                (dest   pathname ,deduce-dest)
                (escape string "@"))))

(define-test-case base-tests task-props ()
  (let ((t (<test-task> 'new #f '() '((src . "a")))))
    (test-compare pathname=? (x->pathname '(() "a")) (t 'prop 'src))
    (test-compare pathname=? (x->pathname '(() ("a" "out"))) (t 'prop 'dest))
    (test-equal "@" (t 'prop 'escape)))
  (let ((t (<test-task> 'new #f '("b") '((src . "a")))))
    (test-compare pathname=? (x->pathname '(() "a")) (t 'prop 'src))
    (test-compare pathname=? (x->pathname '(() "b")) (t 'prop 'dest))
    (test-equal "@" (t 'prop 'escape))))

(define-test-suite (base-tests.project base-tests)
  "Project functionality")

(define-test-case base-tests.project product-dir
  ((setup (assert-clear-stage))
   (teardown
    (delete-file (test-filename "build"))
    (delete-file test-dir)))
  (let* ((project (<project> 'new
                            #f
                            (list (test-filename "build")
                                  (test-filename "src"))
                            '()))
         (result #f)
         (step (object (<step>)
                 ((build self resend)
                  (resend #f 'build)
                  (set! result (working-directory)))))
         (task (object (<task>)
                 (step-prototype step))))
    (project 'add-task (task 'new 'working-dir '() '()))
    (project 'invoke 'build-rec '("working-dir"))
    (test-compare pathname=? (merge-pathnames (pathname-as-directory
                                               (test-filename "build"))
                                              (working-directory))
      result)))

(run-test-suite base-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
