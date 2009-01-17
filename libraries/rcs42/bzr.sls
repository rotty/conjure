;;; bzr.sls --- bzr backend for rcs42

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (rcs42 bzr)
  (export bzr)
  (import (rnrs base)
          (rnrs lists)
          (xitomatl srfi receive)
          (only (xitomatl srfi lists) filter-map)
          (spells operations)
          (spells pathname)
          (spells process)
          (rcs42 operations))

  (define (inventory)
    (filter-map (lambda (f)
                  (and (not (pathname=? (x->pathname f)
                                        (make-pathname #f '() #f)))
                       f))
                (receive (status sig filenames)
                         (run-process/lines #f "bzr" "ls")
                  (if (= 0 status)
                      filenames
                      '()))))

  (define (diff)
    (define (lose msg . irritants)
      (apply error 'bzr-diff msg irritants))
    (receive (status sig lines)
             (run-process/lines #f "bzr" "diff")
      (cond (sig
             (lose "'bzr diff' killed by signal" sig))
            ((not (memv status '(0 1 2)))
             (lose "'bzr diff' exited with unexpected status" status))
            (else
             lines))))

  (define bzr
    (object #f
      ((rcs/inventory self) (inventory))
      ((rcs/diff self)      (diff))))

  )
