;;; config.sls --- Conjure config subsystem

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

;; This is a substitutor task intended to replace autoconf. You add
;; one or more config-subst tasks to your project, declaring what
;; files you want to do config-based substitution in.

;;; Code:
#!r6rs

(library (conjure tasks configure)
  (export <configure-task>)
  (import (rnrs base)
          (rnrs control)
          (rnrs mutable-pairs)
          (rnrs records syntactic)
          (rnrs io simple)
          (rnrs io ports)
          (except (srfi :1 lists) for-each map)
          (srfi :8 receive)
          (srfi :19 time)
          (spells time-lib)
          (spells alist)
          (spells pathname)
          (spells filesys)
          (spells misc)
          (spells match)
          (spells tracing)
          (spells ports)
          (spells logging)
          (prometheus)
          (spells fmt)
          (conjure utils)
          (conjure base))

(define-record-type production
  (fields
   (immutable product)
   (immutable template)
   (mutable fender)))

(define (production-fended? p)
  (cond ((production-fender p)
         => (lambda (fender)
              (if (procedure? fender)
                  (let ((fended? (not (fender))))
                    (production-fender-set! p fended?)
                    fended?)
                  #t)))
        (else #f)))

(define (produce-vprop-setter task value)
  (let ((productions (calc-productions value (task 'prop 'ext))))
    (task 'set-prop! 'productions productions)
    (task 'set-products! (cons (task 'prop 'cache-file)
                               (map production-product productions)))
    (task 'set-sources! (map production-template productions))))

(define-object <configure-step> (<file-step>)
  ((build self resend)
   (let* ((cache-file (self 'prop 'cache-file))
          (escape (self 'prop 'escape))
          (new-cache (rebuild-cache
                      (read-cache cache-file
                                  (map (lambda (fetcher)
                                         (fetcher (self 'project)))
                                       (self 'prop 'fetchers)))
                      (self 'sources)
                      escape)))
     (process-productions (self 'productions) escape new-cache)
     (call-with-output-file/atomic cache-file
       (lambda (port)
         (write-cache port new-cache)))))
  ((clean self resend)
   (delete-file (self 'prop 'cache-file))
   (for-each (lambda (p)
               (delete-file (production-product p)))
             (self 'productions)))
  ((stale? self resend)
   (let ((ext (self 'prop 'ext)))
     (let* ((cache-file (self 'prop 'cache-file))
            (cache-fmt (file-mtime cache-file)))
       (or
         (not cache-fmt)
         (or-map
          (lambda (p)
            (let ((prod (production-product p))
                  (src (production-template p)))
              (or (not (file-exists? prod))
                  (not (file-exists? src))
                  (let ((src-fmt (file-modification-time src)))
                    (time<? cache-fmt src-fmt)
                    (time<? (file-modification-time prod) src-fmt)))))
          (self 'productions)))))))
  
(define-object <configure-task> (<file-task>)
  (properties (append
               `((cache-file (singleton pathname) (",config.cache"))
                 (fetchers (list-of procedure))
                 (escape (singleton string) ("#!@"))
                 (ext (singleton string) ("in"))
                 (produce (virtual (list-of any)) ,produce-vprop-setter))
               (filter-props '(depends)
                             (<file-task> 'properties))))

  (step-prototype <configure-step>)
  ((construct-step self resend project)
   (let ((step (resend #f 'construct-step project)))
     (step 'add-value-slot!
           'productions 
           (updated-productions (self 'prop 'productions)
                                (step 'sources)))
     step)))

(define (calc-productions lst ext)
  (map (lambda (elt)
         (match elt
           ((prod '<= src)
            (make-production (x->pathname prod) (x->pathname src) #f))
           ((prod '<= src ('? fender))
            (make-production (x->pathname prod) (x->pathname src) fender))
           (prod
            (let ((prod (x->pathname prod)))
              (make-production prod (pathname-add-type prod ext) #f)))))
       lst))

;; Returns a list of productions with updated templates (as the final
;; template pathnames are determined at step-construction time).
(define (updated-productions productions templates)
  (map (lambda (p template)
         (make-production (production-product p)
                          template
                          (production-fender p)))
       productions
       templates))

(define (subst-file/cache prod src escape cache)
  (let ((finfo (cache-fileinfo cache src)))
    (cond (finfo
           (log/conf 'info (cat "generating " (dsp-pathname prod)))
           (call-with-input-file (x->namestring src)
             (lambda (in-port)
               (call-with-output-file/atomic (x->pathname prod)
                 (lambda (out-port)
                   (subst-port in-port
                               out-port
                               escape
                               (lambda (port datum)
                                 (put-string out-port (cache-get cache datum)))))))))
          (else
           (log/conf 'info (cat "using " (dsp-pathname src) " verbatim as "
                                (dsp-pathname prod) " (no substitions detected)"))
           (copy-file src prod)))))

(define (process-productions productions escape cache)
  (for-each
   (lambda (p)
     (unless (production-fended? p)
       (let* ((prod (production-product p))
              (prod-fmt (file-mtime prod))
              (src (production-template p))
              (src-fmt (file-mtime src)))
         (if (or (not prod-fmt)
                 (time<? prod-fmt src-fmt))
             (subst-file/cache prod src escape cache)
             (log/conf 'info
                       (cat (dsp-pathname prod) " is up to date"))))))
   productions))

;;; Cache

(define-record-type cache
  (fields fileinfos kvs fetchers))

(define (cache-fileinfo cache pathname)
  (assoc (x->namestring pathname) (cache-fileinfos cache)))

(define (make-fileinfo namestring time)
  (cons namestring (time-utc->posix-timestamp time)))

(define (cache-get cache datum)
  (cond ((assoc datum (cache-kvs cache))
         => (lambda (entry)
              (cond ((cdr entry) => values)
                    (else
                     (update-cache! cache datum)
                     (unless (cdr entry)
                       (error 'cache-get "failed to update cache for datum" datum))
                     (cdr entry)))))
        (else
         (error 'cache-get "unknown datum requested" datum))))

(define (update-cache! cache datum)
  (let ((kvs (cache-kvs cache))
        (results (cache-fetch cache datum)))
    (unless results
      (error 'update-cache! "unable to fetch datum" datum))
    (for-each (lambda (kv)
                (cond ((assoc (car kv) kvs)
                       => (lambda (entry)
                            (set-cdr! entry (cdr kv))))
                      (else
                       (error 'update-cache!
                              "fetcher returned unknown entry" kv))))
              results)))

(define (cache-fetch cache datum)
  (let ((missing (filter-map (lambda (entry)
                               (and (not (cdr entry)) (car entry)))
                         (cache-kvs cache))))
    (or-map (lambda (fetcher)
              (fetcher missing datum))
            (cache-fetchers cache))))

(define fileinfo-namestring car)
(define (fileinfo-time finfo)
  (posix-timestamp->time-utc (cdr finfo)))

(define (read-cache filename fetchers)
  (if (file-exists? filename)
      (call-with-input-file (x->namestring filename)
        (lambda (port)
          (let ((vec (read port)))
            (make-cache (vector-ref vec 0)
                        (vector-ref vec 1)
                        fetchers))))
      (make-cache '() '() fetchers)))

(define (write-cache port cache)
  (write (vector (cache-fileinfos cache)
                 (cache-kvs cache))
         port))

(define (file-mtime file)
  (and (file-exists? file)
       (file-modification-time file)))

(define (rebuild-cache old-cache sources escape)
  (let loop ((finfos '())
             (kvs (cache-kvs old-cache))
             (sources sources))
    (if (null? sources)
        (make-cache finfos kvs (cache-fetchers old-cache))
        (let* ((src (car sources))
               (finfo (cache-fileinfo old-cache src))
               (src-fmt (file-modification-time src)))
          (cond ((or (not finfo)
                     (time<? (fileinfo-time finfo) src-fmt))
                 (loop (cons (make-fileinfo (x->namestring src) src-fmt) finfos)
                       (lset-union (lambda (x1 x2)
                                     (equal? (car x1) (car x2)))
                                   kvs
                                   (map (lambda (e) (cons e #f))
                                        (read-substs src escape)))
                       (cdr sources)))
                (else
                 (loop (cons finfo finfos) kvs (cdr sources))))))))

(define (read-substs pathname escape)
  (log/conf 'info "scanning " (dsp-pathname pathname))
  (call-with-input-file (x->namestring pathname)
    (lambda (port)
      (fold-escapes (lambda (datum finfos)
                      (cons datum finfos))
                    (lambda (buffer start end finfos)
                      finfos)
                    '()
                    port
                    escape))))

(define logger:conjure.configure (make-logger logger:conjure 'configure))
(define log/conf (make-fmt-log logger:conjure.configure))

)
