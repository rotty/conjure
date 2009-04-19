;;; host-info.sls --- Fetcher for information about the host

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

(library (conjure hostinfo)
  (export hostinfo-fetcher)
  (import (rnrs base)
          (rnrs lists)
          (only (srfi :1) filter-map)
          (srfi :8 receive)
          (spells match)
          (spells sysutils))

(define (known-name? name)
  (and (memq name '(cpu vendor os))))

(define (hostinfo-fetcher)
  (lambda (project)
    (lambda (missing datum)
      (match datum
        (('host-info (? known-name?))
         (receive (cpu vendor os) (host-info)
           (filter-map (lambda (item)
                         (let ((val (match item
                                      (('host-info 'cpu)    cpu)
                                      (('host-info 'vendor) vendor)
                                      (('host-info 'os)     os)
                                      (_
                                       #f))))
                           (and val (cons item val))))
                       missing)))
        (_
         #f)))))

)
