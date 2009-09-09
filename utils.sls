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
  (export list-intersperse

          send-all
          object

          coerce
          build-failure
          raise-task-error
          last-modification-time
          all-files-exist?
          invert-dag
          pathname-strip-type
          pathname-add-type

          fold-escapes
          subst-port

          vprop-setter
          vprop-setter/dir
          propinfo-type
          split-props
          filter-props

          make-fmt-log
          dsp-obj
          dsp-pathname
          dsp-time-utc)
  (import (rnrs base)
          (rnrs control)
          (rnrs syntax-case)
          (rnrs hashtables)
          (rnrs io ports)
          (rnrs mutable-strings)
          (except (srfi :1) for-each map)
          (srfi :8 receive)
          (only (srfi :13 strings)
                string-suffix? string-copy!)
          (srfi :19 time)
          (only (srfi :43 vectors) vector-fold)
          (spells misc)
          (spells match)
          (spells pathname)
          (spells tracing)
          (spells filesys)
          (spells logging)
          (prometheus)
          (spells fmt))

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
    (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
      (if (null? l) (reverse dest)
        (loop (cdr l) (cons (car l) (cons elem dest)))))))

(define (send-all objects msg . args)
  (for-each (lambda (o) (apply o msg args)) objects))

(define-syntax object
  (syntax-rules ()
    ((object clause ...)
     (let ()
       (define-object obj clause ...)
       obj))))

(define (coerce val type)
  (define (lose)
    (error 'coerce "value is not of expected type" val type))
  (define (checked pred)
    (unless (pred val)
      (lose))
    val)
  (match type
    ('pathname  (x->pathname val))
    ('string    (checked string?))
    ('procedure (checked procedure?))
    ('number    (checked number?))
    ('integer   (checked integer?))
    ('symbol    (checked symbol?))
    ('any       val)
    (('list-of elt-type)
     (map (lambda (v)
            (coerce v elt-type))
          (checked list?)))
    (('singleton elt-type)
     (unless (and (pair? val)
                  (null? (cdr val)))
       (lose))
     (coerce (car val) elt-type))
    (else
     (error 'coerce "invalid type" type))))

(define (propinfo-type propinfo)
  (match (cadr propinfo)
    (('virtual type) type)
    ('virtual        'any)
    (type            type)))

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
        (pathname-with-file pathname
                            (make-file (file-name file) (drop-right types 1)))
        pathname)))

(define (pathname-add-type pathname type)
  (let ((file (pathname-file pathname)))
    (pathname-with-file pathname
                        (make-file (file-name file)
                                   (append (file-types file) (list type))))))

(define (fold-escapes f0 f1 seed port escape)
  (let* ((esc-len (string-length escape))
         (buf-size (max 4000 (* esc-len 16)))
         (buffer (make-string buf-size)))
    (let loop ((idx 0) (seed seed))
      (let ((c (get-char port)))
        (cond ((eof-object? c)
               (f1 buffer 0 idx seed))
              (else
               (string-set! buffer idx c)
               (let ((idx (+ idx 1)))
                 (cond ((and (>= idx esc-len)
                              (string-suffix? escape buffer
                                              0 esc-len 0 idx))
                        (let ((datum (get-datum port)))
                          (cond ((eof-object? datum)
                                 (f1 buffer 0 idx seed))
                                (else
                                 (loop 0 (f0 datum
                                             (f1 buffer 0 (- idx esc-len) seed)))))))
                       ((< idx buf-size)
                        (loop idx seed))
                       (else
                        (let ((seed (f1 buffer 0 (- idx esc-len) seed)))
                          (string-copy! buffer 0 buffer (- idx esc-len) idx)
                          (loop esc-len seed)))))))))))

(define (subst-port in-port out-port escape replacer)
  (fold-escapes (lambda (datum seed)
                  (replacer out-port datum)
                  seed)
                (lambda (buffer start end seed)
                  (put-string out-port buffer start end)
                  seed)
                (unspecific)
                in-port
                escape))

(define vprop-setter
  (case-lambda
    ((slot-setter coerce)
     (lambda (self value)
       (self slot-setter (coerce value))))
    ((slot-setter)
     (lambda (self value)
       (self slot-setter value)))))

(define (vprop-setter/dir slot-setter)
  (lambda (self v)
    (self slot-setter (pathname-as-directory (x->pathname v)))))

(define (split-props props)
  (let loop ((pos-props '())
             (tagged-props '())
             (props props))
    (if (null? props)
        (values (reverse pos-props) (reverse tagged-props))
        (syntax-case (car props) ()
          ((name val0 vals ...)
           (loop pos-props
                 (cons #'(cons 'name (list val0 vals ...)) tagged-props)
                 (cdr props)))
          ((name . val)
           (loop pos-props (cons #'`(name . ,val) tagged-props) (cdr props)))
          (val
           (loop (cons #'val pos-props) tagged-props (cdr props)))))))

(define (filter-props names props)
  (filter (lambda (prop)
            (memq (car prop) names))
          props))

(define (make-fmt-log logger)
  (let ((log (make-log logger)))
    (lambda (level . formats)
      (log level (lambda (port)
                   (apply fmt port formats))))))

(define (dsp-obj obj)
  (lambda (st)
    (if (fmt-ref st 'dsp-obj-full?)
        ((obj 'wrt) st)
        ((obj 'dsp) st))))

(define (dsp-pathname pathname)
  (lambda (st)
    ((dsp (x->namestring pathname)) st)))

(define (dsp-time-utc time)
  (let* ((date (time-utc->date time))
         (fmt (cond ((= (date-year date) (date-year (current-date)))
                    "~b ~e ~H:~M:~S")
                   (else
                    "~b ~e ~Y"))))
    (date->string date fmt)))

)
