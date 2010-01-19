;;; git.sls --- Git interface

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (conjure rcs git)
  (export git)
  
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs control)
          (rnrs lists)
          (rnrs io simple)
          (rnrs io ports)
          (only (srfi :1 lists) filter-map)
          (srfi :8 receive)
          (srfi :13 strings)
          (spells pathname)
          (spells filesys)
          (spells process)
          (spells sysutils)
          (spells delimited-readers)
          (spells misc)
          (spells opt-args)
          (spells operations)
          (rename (only (conjure utils) object)
                  (object obj))
          (conjure rcs utils)
          (conjure rcs prompt)
          (conjure rcs operations))

(define <git-runner> (make-cmd-runner "git"))
(define <git-runner/log> (make-logged-runner <git-runner>))
(define <git-runner/stdout> (make-stdout-runner <git-runner>))

(define (run-git/log command . args)
  (<git-runner/log> 'run (cons (symbol->string command) args)))

(define inventory
  (let ((runner (obj (<git-runner/stdout>) (stdout 'lines))))
    (lambda ()
      (filter-map (lambda (f)
                    (and (not (pathname=? (x->pathname f)
                                          (make-pathname #f '() #f)))
                         f))
                  (runner 'run '("ls-files"))))))

(define diff
  (let ((runner (obj (<git-runner>)
                  (stdout 'lines)
                  (success-codes '(0 1))
                  ((term-successful self resend argv status stdout stderr)
                   (case status
                     ((0) stdout)
                     ((1)  ; throw away output if there were no changes
                      (cdr stdout)))))))
    (lambda ()
      (runner 'run '("diff" "--exit-code" "HEAD")))))

(define git
  (object #f
    ((rcs/pull self repo branch)
     (run-git/log 'pull "--no-commit" repo (or branch "master")))
    ((rcs/push self repo branch)
     (run-git/log 'push repo (or branch "master")))
    ((rcs/inventory self)
     (inventory))
    ((rcs/diff self)
     (diff))
    ((rcs/get self repo dir)
     (run-git/log 'clone repo dir))))

)

;; Local Variables:
;; scheme-indent-styles: ((object 1) (obj 1))
;; End:
