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
          raise-task-error
          last-modification-time
          all-files-exist?
          invert-dag
          pathname-strip-type
          pathname-add-type
          subst-port)
  (import (rnrs base)
          (rnrs hashtables)
          (rnrs io ports)
          (rnrs mutable-strings)
          (srfi :1 lists)
          (srfi :8 receive)
          (only (srfi :13 strings)
                string-suffix? string-copy!)
          (srfi :19 time)
          (only (srfi :43 vectors) vector-fold)
          (spells pathname)
          (spells filesys))

(define (send-all objects msg . args)
  (for-each (lambda (o) (apply o msg args)) objects))

(define (build-failure msg . args)
  (apply error 'build-failure msg args))

(define (raise-task-error who msg . args)
  ;; TODO: use conditions
  (apply error who msg args))

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

(define (pathname-strip-type pathname type)
  (let* ((file (pathname-file pathname))
         (types (file-types file)))
    (if (and (not (null? types))
             (equal? (last types) type))
        (values
         (pathname-with-file pathname
                             (make-file (file-name file) (drop-right types 1)))
         #t)
        (values pathname #f))))

(define (pathname-add-type pathname type)
  (let ((file (pathname-file pathname)))
    (pathname-with-file pathname
                        (make-file (file-name file)
                                   (append (file-types file) (list type))))))

(define (subst-port in-port out-port escape replacer)
  (let* ((esc-len (string-length escape))
         (buf-size (max 4000 (* esc-len 16)))
         (buffer (make-string buf-size)))
    (let loop ((idx 0))
      (let ((c (get-char in-port)))
        (cond ((eof-object? c)
               (put-string out-port buffer 0 idx))
              (else
               (string-set! buffer idx c)
               (let ((idx (+ idx 1)))
                 (cond ((and (>= idx esc-len)
                              (string-suffix? escape buffer
                                              0 esc-len 0 idx))
                        (let ((datum (get-datum in-port)))
                          (cond ((eof-object? datum)
                                 (put-string out-port buffer 0 idx))
                                (else
                                 (put-string out-port buffer 0 (- idx esc-len))
                                 (replacer out-port datum)
                                 (loop 0)))))
                       ((< idx buf-size)
                        (loop idx))
                       (else
                        (put-string out-port buffer 0 (- idx esc-len))
                        (string-copy! buffer 0 buffer (- idx esc-len) idx)
                        (loop esc-len))))))))))

)
