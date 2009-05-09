;;; utils.scm --- Unit test for (conjure utils)

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

(define-test-suite utils-tests
  "Utilities")

(define-test-case utils-tests pathname-strip-type ()
  (test-compare pathname=? (make-pathname #f '() "bar") 
    (pathname-strip-type (x->pathname "bar.scm") "scm"))
  (test-compare pathname=? (make-pathname #f '() (make-file "foxy" "lady"))
    (pathname-strip-type (x->pathname "foxy.lady") "scm")))

(define (make-subst-string n pos middle)
  (string-append (make-string pos #\x)
                 middle
                 (make-string (- n pos) #\y)))

(define (test-subst n pos middle escape replacement)
  (let ((in-port (open-string-input-port (make-subst-string n pos "@(1)"))))
    (test-equal (make-subst-string n pos replacement)
      (call-with-string-output-port
        (lambda (out-port)
          (subst-port in-port out-port escape
                      (lambda (port x)
                        (put-string port replacement))))))))

(define-test-case utils-tests subst-port ()
  (test-subst 80 40 "@(1)" "@" "")
  (test-subst 9000 3999 "@(1)" "@" "1234567"))

(run-test-suite utils-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
