;;; conjure.sps --- Conjure driver program

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

(import (rnrs)
        (prometheus)
        (spells process)
        (spells pathname)
        (conjure base)
        (conjure file-goal))

(define (make-test-goal filename . sources)
  (define-object g (<file-goal>)
    (products (list filename))
    (sources sources)
    ((construct-task self resend register)
     (let ((task (resend #f 'construct-task register)))
       (task 'add-method-slot! 'build
             (lambda (self resend)
               (for-each (lambda (prod)
                           (for-each display (list "touching: " filename "\n"))
                           (run-process #f "/bin/touch" prod))
                         (task 'products))))
       task)))
  g)

(define (main)
  (let ((r (<register> 'new 'test-register "." ",build/")))
    (r 'add-goal (make-test-goal ",a-file" ",another-file"))
    (r 'add-goal (make-test-goal ",another-file"))
    (r 'build-rec)))

(main)
