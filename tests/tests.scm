;;; tests.scm --- List of tests for conjure

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

((systems conjure)
 (files
  ("utils.scm"
   (rnrs io ports)
   (srfi :8 receive)
   (spells pathname)
   (conjure utils))
  ("base.scm"
   (spells pathname)
   (spells tracing)
   (prometheus)
   (conjure utils)
   (conjure base))
  ("dsl.scm"
   (only (spells assert) cout)
   (conjure base)
   (conjure task-lib)
   (conjure dsl))))
