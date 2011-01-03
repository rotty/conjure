;;; dorodango.sls --- Dorodango support for conjure

;; Copyright (C) 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (conjure dorodango)
  (export make-conjure-hook)
  (import (rnrs)
          (spells logging)
          (conjure task-lib)
          (conjure dsl))

(define (make-conjure-hook proc)
  (register-builtin-tasks)
  (lambda (agent)
    (let-logger-properties
        ((root-logger
          `((threshold info)
            (handlers
             ,(lambda (entry)
                (default-log-formatter entry (current-error-port)))))))
     (let ((p (project (agent 'package-name)
                  ((source-dir (agent 'unpacked-source))
                   (product-dir (agent 'unpacked-source)))
                (proc agent))))
       (p 'invoke 'build-rec '())))))

)

;; Local Variables:
;; scheme-indent-styles: (conjure-dsl (let-logger-properties 1))
;; End:
