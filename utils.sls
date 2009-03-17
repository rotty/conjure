;;; utils.sls --- Utility functions for conjure

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

(library (conjure utils)
  (export send-all
          build-failure
          last-modification-time
          all-files-exist?
          invert-dag)
  (import (rnrs base)
          (rnrs hashtables)
          (srfi :1 lists)
          (srfi :8 receive)
          (srfi :19 time)
          (spells filesys)
          (only (srfi :43 vectors) vector-fold))

(define (send-all objects msg . args)
  (for-each (lambda (o) (apply o msg args)) objects))

(define (build-failure msg . args)
  (apply error 'build-failure msg args))

(define (last-modification-time files)
  (fold (lambda (file time)
          (let ((fmt (file-modification-time file)))
            (cond ((and time (time>? time fmt)) time)
                  (else                         fmt))))
        #f
        files))

(define (all-files-exist? files)
  (and (every file-exists? files) #t))

(define (invert-dag dag)
  (let ((tbl (make-eqv-hashtable)))
    (for-each (lambda (adj)
                (let ((vertex (car adj)))
                  (hashtable-update! tbl vertex values '())
                  (for-each (lambda (v)
                              (hashtable-update! tbl
                                                 v
                                                 (lambda (prev)
                                                   (cons vertex prev))
                                                 '()))
                            (cdr adj))))
              dag)
    (receive (keys values) (hashtable-entries tbl)
      (vector-fold (lambda (i state k v)
                     (cons (cons k v) state))
                   '()
                   keys
                   values))))
)
