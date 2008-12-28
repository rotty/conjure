;;; config.sls --- Handling of "configs", as pioneered by GNU Arch

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

(library (rcs42 config)
  (export build-config
          config-inventory
          config-whatsnew
          config-dist)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs io simple)
          (spells receive)
          (only (spells lists) fold)
          (only (spells strings) substring/shared string-index-right)
          (spells opt-args)
          (spells misc)
          (spells filesys)
          (spells process)
          (spells include)
          (rcs42 utils)
          (rcs42 files)
          (rcs42 prompt)
          (rcs42 operations)
          (rcs42 default))

  (include-file ((rcs42 scheme) config)))
