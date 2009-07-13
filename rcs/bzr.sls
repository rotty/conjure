;;; bzr.sls --- bzr backend for rcs42

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (conjure rcs bzr)
  (export bzr)
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs lists)
          (rnrs io simple)
          (rnrs io ports)
          (srfi :8 receive)
          (only (srfi :1 lists) filter-map)
          (srfi :13 strings)
          (spells operations)
          (spells pathname)
          (spells process)
          (spells sysutils)
          (conjure run)
          (rename (only (conjure utils) object)
                  (object obj))
          (conjure rcs utils)
          (conjure rcs operations))

  (define <bzr-runner> (make-cmd-runner "bzr"))
  (define <bzr-runner/log> (make-logged-runner <bzr-runner>))
  (define <bzr-runner/stdout> (make-stdout-runner <bzr-runner>))

  (define (run-bzr/log command . args)
    (<bzr-runner/log> 'run (cons (symbol->string command) args)))

  (define inventory
    (let ((runner (obj (<bzr-runner/stdout>)
                    (stdout 'lines))))
      (lambda ()
        (filter-map (lambda (f)
                      (and (not (pathname=? (x->pathname f)
                                            (make-pathname #f '() #f)))
                           f))
                    (runner 'run '("ls" "--recursive"))))))

  (define diff
    (let ((runner (obj (<bzr-runner/stdout>)
                    (stdout 'lines)
                    (success-codes '(0 1 2)))))
      (lambda ()
        (runner 'run '("diff")))))

  (define info
    (let ((runner (obj (<bzr-runner/stdout>)
                    (stdout 'lines))))
      (lambda ()
        (runner 'run '("info")))))

  (define (lightweight?)
    (and (exists (lambda (line)
                   (string-prefix? "Lightweight checkout" line))
                 (info))
         #t))

  (define bzr
    (object #f
      ((rcs/pull self repo branch)
       (if (lightweight?)
           (run-bzr/log 'update)
           (run-bzr/log 'pull repo)))
      ((rcs/inventory self) (inventory))
      ((rcs/diff self)      (diff))))

  )

;; Local Variables:
;; scheme-indent-styles: ((object 1) (obj 1))
;; End:
