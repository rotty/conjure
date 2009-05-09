;;; cc.sls --- Conjure C compiler support

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
          (rnrs io ports)
          (except (srfi :1 lists) for-each map)
          (srfi :8 receive)
          (only (srfi :13 strings) string-map)
          (spells alist)
          (spells match)
          (spells pathname)
          (spells filesys)
          (spells sysutils)
          (spells process)
          (spells tracing)
          (only (spells assert) cout)
          (prometheus)
          (fmt)
          (fmt c)
          (conjure base)
          (conjure run)
          (conjure utils))

(define-object <cc> (*the-root-object*)
  (%cached-program %set-cached-program! #f)
  ((program-path self)
   (or (self '%cached-program)
       (let ((path (find-exec-path (self 'program))))
         (unless path
           (build-failure "program not found" (self 'program)))
         (self '%set-cached-program! path)
         path)))
  
  (program "gcc")
  
  (runner (<runner> 'clone))
  (include-paths %set-include-paths! '())

  ((log self resend level msg)
   (log/cc level msg))
  
  ((add-include-path! self resend path)
   (self '%set-include-paths! (append (self 'include-paths)
                                      (list (x->pathname path)))))
  ((compile-object self resend dest src)
   (self 'log 'info (cat "compiling object " (dsp-pathname src)))
   ((self 'runner) 'run (cons (self 'program-path)
                              (self 'compile-object-args dest src))))

  ((compile-program self resend dest srcs)
   (self 'log 'info (cat "compiling program " (dsp-pathname dest)
                         " <= " (fmt-join dsp-pathname srcs ", ")))
   ((self 'runner) 'run (cons (self 'program-path)
                              (self 'compile-program-args dest srcs)))))

(define-object <gcc> (<cc>)
  ((cpp-flags self resend)
   (list-intersperse (self 'include-paths) "-I"))

  ((c-flags self resend)
   '())
  
  ((ld-flags self resend)
   '())
  
  ((compile-object-args self resend dest src)
   (append (self 'cpp-flags) (self 'c-flags) (list "-o" dest src)))

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

(define (cc-probe-sizeof missing c-src)
  (fold (lambda (expr c-src)
          (match expr
            (('c-sizeof type . headers)
             (extend-c-src c-src
                           (map (lambda (hdr)
                                  `(%include ,(dsp hdr)))
                                headers)
                           (list (cc-out-sizeof type expr))))
            (_
             c-src)))
        c-src
        missing))

(define (cc-out-sizeof type expr)
  `(printf ,(string-append "(" (fmt #f (wrt expr)) " . \"%lu\")\n")
           (%cast (unsigned long) (sizeof ,type))))

(define cc-probes
  `((c-sizeof ,cc-probe-sizeof)))

(define (cc-fetcher cc-name)
  (lambda (project)
    (let ((cc ((project 'get-step cc-name) 'build)))
      (lambda (missing datum)
        (cond ((and (pair? datum)
                    (assq-ref cc-probes (car datum)))
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

(define-object <cc-conf-task> (<task>)
  ((construct-step self resend project)
   (object (<step> (task self))
     (%cc %set-cc! #f)
     (project project)
     ((build self resend)
      (resend #f 'build)
      (cond ((self '%cc)
             => values)
            (else
             (let ((cc (cc-conf self)))
               (self '%set-cc! cc)
               cc)))))))

(define (cc-conf step)
  (let ((gcc-path (find-exec-path "gcc")))
    (unless gcc-path
      (build-failure "cannot find gcc in path"))
    (object (<gcc>)
      (program-path gcc-path))))



(define log/cc (make-fmt-log '(conjure cc)))

)

