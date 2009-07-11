;;; git.sls --- Git interface

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
          (xitomatl irregex)
          (conjure rcs utils)
          (conjure rcs files)
          (conjure rcs prompt)
          (conjure rcs operations))

(define git-command
  (or (find-exec-path "git")
      (error 'git-command "git executable not found")))

(define (run-git command . args)
  (define (lose msg . irritants)
    (apply error
           'run-git
           (string-append "git command '" (symbol->string command) "' " msg)
           irritants))
  (receive (status sig)
           (apply run-process #f git-command (symbol->string command) args)
    (cond (sig
           (lose "killed by signal" sig))
          ((not (= status 0))
           (lose "exited with non-zero status" status)))))

(define (run-git/log command . args)
  (log-cmd-line (cons* "git" command args))
  (apply run-git command args))

(define (inventory)
  (filter-map (lambda (f)
                (and (not (pathname=? (x->pathname f)
                                      (make-pathname #f '() #f)))
                     f))
              (receive (status sig filenames)
                       (run-process/lines #f git-command "ls-files")
                (if (= 0 status)
                    filenames
                    '()))))

(define (diff)
  (define (lose msg . irritants)
    (apply error 'git-diff msg irritants))
  (receive (status sig lines)
           (run-process/lines #f git-command "diff" "--exit-code" "HEAD")
    (cond (sig
           (lose "'git diff' killed by signal" sig))
          ((not (memv status '(0 1)))
           (lose "'git diff' exited with unexpected status" status))
          ((= status 1)
           (cdr lines)) ;; throw away output if there were no changes
          (else
           lines))))

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
