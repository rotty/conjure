;;; file-task.sls --- A task operating on files

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
(library (conjure file-task)
  (export <file-task>)
  (import (rnrs base)
          (srfi :1 lists)
          (srfi :19 time)
          (spells pathname)
          (spells filesys)
          (conjure utils)
          (conjure base)
          (conjure usual)
          (prometheus))

(define-object <file-task> (<usual-task>)
  ((construct-step self resend project)
   (define (file-check source)
     (lambda ()
       (let ((filename (pathname-join (project 'source-dir) source)))
         (cond ((file-exists? filename)
                #f)
               (else
                (build-failure "no step found for building {0}"
                               (x->namestring filename)))))))
   (let* ((source-dir (project 'source-dir))
          (prod-dir (project 'product-dir))
          (sources (map (lambda (src)
                          (pathname-join (if (project 'has-product? src)
                                             prod-dir
                                             source-dir)
                                         src))
                        (self 'sources)))
          (products (map (lambda (prod)
                           (pathname-join prod-dir prod))
                         (self 'products))))
     (define-object step (<usual-step> (task self))
       (dependencies (append (map (lambda (dep)
                                    (project 'get-step dep))
                                  (self 'dependencies))
                             (filter-map
                              (lambda (source)
                                (project 'get-step source (file-check source)))
                              (self 'sources))))
       (sources sources)
       (products products)
       ((stale? self resend)
        (or (not (all-files-exist? products))
            (not (all-files-exist? sources))
            (and (not (or (null? sources) (null? products)))
                 (time>? (last-modification-time sources)
                         (last-modification-time products))))))
     step)))

)
