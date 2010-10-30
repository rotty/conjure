;;; cc.sls --- Conjure C compiler support

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

;;; Code:
#!r6rs

(library (conjure cc)
  (export <cc> <gcc>
          cc-fetcher
          <cc-conf-task>)
  (import (rnrs base)
          (rnrs control)
          (rnrs records syntactic)
          (rnrs hashtables)
          (rnrs io ports)
          (except (srfi :1 lists) for-each map)
          (srfi :8 receive)
          (only (srfi :13 strings) string-map)
          (spells define-values)
          (spells alist)
          (spells match)
          (spells pathname)
          (spells filesys)
          (spells sysutils)
          (spells process)
          (spells tracing)
          (spells logging)
          (wak fmt)
          (wak fmt c)
          (wak prometheus)
          (conjure base)
          (conjure run)
          (conjure utils))

(define-object <cc> (<program>)
  (program "cc")
  
  (runner (<runner> 'clone))
  (include-paths %set-include-paths! '())

  ((log self resend level msg)
   (log/cc level msg))
  
  ((add-include-path! self resend path)
   (self '%set-include-paths! (append (self 'include-paths)
                                      (list (x->pathname path)))))
  ((compile-object self resend dest srcs)
   (self 'log 'info (cat "compiling object " (dsp-pathname dest)))
   ((self 'runner) 'run (cons (self 'program-path)
                              (self 'compile-object-args dest srcs))))

  ((compile-program self resend dest srcs)
   (self 'log 'info (cat "compiling program " (dsp-pathname dest)
                         " <= " (fmt-join dsp-pathname srcs ", ")))
   ((self 'runner) 'run (cons (self 'program-path)
                              (self 'compile-program-args dest srcs)))))

(define-object <gcc> (<cc>)
  (program "gcc")
  
  ((cpp-flags self resend)
   (list-intersperse (self 'include-paths) "-I"))

  ((c-flags self resend)
   '())
  
  ((ld-flags self resend)
   '())
  
  ((compile-object-args self resend dest srcs)
   (append '("-c") (self 'cpp-flags) (self 'c-flags) (cons* "-o" dest srcs)))

  ((compile-program-args self resend dest srcs)
   (append (list "-o" dest)
           (self 'cpp-flags) (self 'c-flags) (self 'ld-flags) srcs)))

;;; The autoconfish part

(define-record-type c-src
  (fields global-decls
          stmts))

(define (extend-c-src c-src global-decls stmts)
  (make-c-src (append global-decls (c-src-global-decls c-src))
              (append stmts (c-src-stmts c-src))))

(define (fmt-c-src port c-src)
  (fmt port (c-expr `(%include ,(dsp "stdio.h"))))
  (fmt port (fmt-join c-expr (delete-duplicates (c-src-global-decls c-src)
                                                equal?)))
  (fmt port (c-expr `(%fun int main ((int argc) ((char * *) argv))
                           ,@(c-src-stmts c-src)
                           0))))

(define (headers->includes headers)
  (map (lambda (hdr)
         `(%include ,hdr))
       headers))

(define (cc-out-ulong key expr)
  `(printf ,(string-append "(" (fmt #f (wrt key)) " . \"%lu\")\n")
           (%cast (unsigned long) ,expr)))

(define cc-gensym
  (let ((counter 0))
    (lambda (prefix)
      (set! counter (+ counter 1))
      (string->symbol
       (string-append (symbol->string prefix)
                      (number->string counter))))))

(define cc-probes (make-eq-hashtable))
(define cc-probe-categories (make-eq-hashtable))

(define (lookup-cc-probes name)
  (cond ((hashtable-ref cc-probes name #f)
         => (lambda (category)
              (hashtable-ref cc-probe-categories category '())))
        (else
         '())))

(define-syntax define-cc-probe
  (syntax-rules (category)
    ((_ (<name> . <pattern>) (category <category>)
        <global-expr> <out-proc> ...)
     (define-values ()
       (let ()
         (define (probe missing c-src)
           (fold (lambda (expr c-src)
                   (match expr
                     (('<name> . <pattern>)
                      (extend-c-src c-src
                                    <global-expr>
                                    (list (<out-proc> expr) ...)))
                     (_
                      c-src)))
                 c-src
                 missing))
         (hashtable-set! cc-probes '<name> '<category>)
         (hashtable-update! cc-probe-categories
                            '<category>
                            (lambda (probes)
                              (cons probe probes))
                            '()))))))

(define (cc-fetcher cc-name)
  (lambda (project)
    (let ((cc ((project 'get-step cc-name) 'build)))
      (lambda (missing datum)
        (cond ((and (pair? datum)
                    (lookup-cc-probes (car datum)))
               => (lambda (probes)
                    (let ((c-src
                           (fold-right (lambda (probe c-src)
                                         (probe missing c-src))
                                       (make-c-src '() '())
                                       probes)))
                      (let-values (((c-src-path c-src-port)
                                    (create-temp-file '(() (",config-fetch" "c"))))
                                   ((prog-path prog-port)
                                    (create-temp-file ",config-fetch")))
                        (close-port prog-port)
                        (call-with-port c-src-port
                          (lambda (port)
                            (fmt-c-src port c-src)))
                        (cc 'compile-program prog-path (list c-src-path))
                        (delete-file c-src-path)
                        ((object (<runner>)
                           (stdout 'sexps)
                           ((term-successful self resend argv status stdout stderr)
                            (delete-file prog-path)
                            stdout)) 'run (list prog-path))))))
              (else
               #f))))))

(define-object <cc-conf-step> (<step>)
  (%cc %set-cc! #f)
  
  ((build self resend)
   (resend #f 'build)
   (cond ((self '%cc)
          => values)
         (else
          (let ((cc (cc-conf self)))
            (self '%set-cc! cc)
            cc)))))

(define-object <cc-conf-task> (<task>)
  (step-prototype <cc-conf-step>))

(define (cc-conf step)
  (let ((gcc-path (find-exec-path "gcc")))
    (unless gcc-path
      (build-failure "cannot find gcc in path"))
    (object (<gcc>)
      (program-path gcc-path))))

(define logger:conjure.cc (make-logger logger:conjure 'cc))
(define log/cc (make-fmt-log logger:conjure.cc))



;; The actual probes
(define-cc-probe (c-sizeof type . headers)
  (category base)
  (headers->includes headers)
  (lambda (key)
    (cc-out-ulong key `(sizeof ,(c-type type)))))

(define-cc-probe (c-alignof type . headers)
  (category base)
  (headers->includes (cons "stddef.h" headers))
  (lambda (key)
    `(%block ""
       (typedef (struct ((char x)
                         (,type y)))
                cc__alignof_)
       ,(cc-out-ulong key `(offsetof cc__alignof_ y)))))

)

;; Local Variables:
;; scheme-indent-styles: ((match 1) (%block 1))
;; End:
