;;; config.sls --- Handling of "configs", as pioneered by GNU Arch

;; Copyright (C) 2008, 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (conjure rcs config)
  (export build-config
          config-inventory
          config-diff)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs io simple)
          (srfi :8 receive)
          (only (srfi :1) drop fold unfold append-map append-reverse)
          (only (srfi :13 strings) substring/shared string-index-right)
          (spells opt-args)
          (spells misc)
          (spells match)
          (spells pathname)
          (spells filesys)
          (spells process)
          (spells sysutils)
          (spells irregex)
          (spells include)
          (spells tracing)
          (spells foof-loop)
          (conjure utils)
          (conjure rcs utils)
          (conjure rcs prompt)
          (conjure rcs operations)
          (conjure rcs default))

(define (normalize-config config)
  (define (lose form)
    (error 'normalize-config "config file must be of the form ((dir repo) ...)"
           form))
  (unless (list? config) (lose "malformed config" config))
  (map (lambda (prj)
         (let ((l (length prj)))
           (cond 
              ((= l 2) (list (default-rcs) (car prj) (cadr prj) '()))
              ((> l 2)
               (list (get-rcs (cadr prj)) (car prj) (caddr prj) (cdddr prj)))
              (else
               (lose prj)))))
       config))

(define (config-fold kons nil cfg)
  (define (lose msg . irritants)
    (apply error 'config-fold msg irritants))
  (unless (file-readable? cfg)
    (lose "file not readable" cfg))
  (fold (lambda (prj rest)
          (kons (car prj)
                (pathname-as-directory (cadr prj))
                (caddr prj)
                (cadddr prj)
                rest))
        nil
        (normalize-config (call-with-input-file cfg read))))

(define (config-for-each proc cfg)
  (config-fold (lambda (rcs dir repo opts rest)
                 (apply proc rcs dir repo opts)
                 rest)
               (unspecific)
               cfg))

(define (build-config cfg . args)
  (let-optionals* args ((mode 'ask))
    (config-for-each
     (lambda (rcs dir repo . opts)
       (let-optionals* opts ((branch #f))
         (define (pull)
           (with-working-directory dir
             (lambda () (rcs/pull rcs repo branch))))
         (define (fresh)
           ;; Move directory out of the way
           (temp-pathname-iterate
            (lambda (tmp next) (rename-file dir tmp) tmp)
            dir)
           (rcs/get rcs repo dir))
         (define (push)
           (with-working-directory dir
             (lambda () (rcs/push rcs repo branch))))
         (if (file-exists? dir)
             (case mode
               ((ask)
                (choose (string-append (x->namestring dir) " exists: ")
                        #f
                        `((,(string-append "Update pulling from "
                                           repo)
                           ,pull)
                          ("Get a fresh copy" ,fresh))))
               ((pull) (pull))
               ((fresh) (fresh))
               ((push)  (push)))
             (rcs/get rcs repo dir))))
     cfg)))

(define (config-inventory cfg)
  (config-fold
   (lambda (rcs dir repo opts rest)
     (let ((file-list (with-working-directory dir
                        (lambda ()
                          (map (lambda (filename)
                                 (pathname-join dir filename))
                               (rcs/inventory rcs))))))
       (append rest (if (empty-pathname? dir)
                        file-list
                        (cons dir file-list)))))
   '()
   cfg))

(define (config-diff cfg . args)
  (reverse
   (config-fold
    (lambda (rcs dir repo opts rest)
      (with-working-directory dir
        (lambda ()
          (append-reverse (augment-diff dir 1 (rcs/diff rcs))
                          rest))))
    '()
    cfg)))

(define diff-file-header-irx
  (irregex '(: bos (=> head (or "---" "+++"))
               " " (=> filename (+ (~ space)))
               (=> rest (* any)))))

(define (augment-diff directory strip lines)
  (define (augment-line line)
    (cond ((irregex-search diff-file-header-irx line)
           => (irx-match-lambda (head filename rest)
                (augment-header head filename rest)))
          (else
           line)))
  (define (augment-header head filename rest)
    (let ((pathname (->pathname filename)))
      (string-append
       head
       " "
       (if (pathname-origin pathname)
           filename
           (let ((stripped
                  (make-pathname #f
                                 (drop (pathname-directory pathname) strip)
                                 (pathname-file pathname))))
             (->namestring (merge-pathnames stripped directory))))
       rest)))
  (loop ((for line (in-list lines))
         (for result (listing (augment-line line))))
    => result))

)

;; Local Variables:
;; scheme-indent-styles: ((irx-match-lambda 1))
;; End:
