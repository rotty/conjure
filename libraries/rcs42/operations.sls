;;; operations.sls --- Basic RCS operations

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

(library (rcs42 operations)
  (export rcs/get
          rcs/pull
          rcs/push
          rcs/inventory
          rcs/diff)
  (import (rnrs base)
          (spells operations))

  (define-operation (rcs/get rcs repo dir))
  (define-operation (rcs/pull rcs repo))
  (define-operation (rcs/push rcs repo))
  (define-operation (rcs/inventory rcs))
  (define-operation (rcs/diff rcs))
  
  )
