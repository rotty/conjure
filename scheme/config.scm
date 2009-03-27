;;; config.scm --- Handling of "configs", as pioneered by GNU Arch

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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


(define (normalize-config config)
  (define (lose form)
    (error 'normalize-config "config file must be of the form ((dir repo) ...)"
           form))
  (unless (list? config) (lose "malformed config" config))
  (map (lambda (prj)
         (let ((l (length prj)))
           (unless (and (list? prj) (<= 2 l 3))
             (lose prj))
           (case l
              ((2) (list (default-rcs) (car prj) (cadr prj)))
              ((3) (list (get-rcs (cadr prj)) (car prj) (caddr prj)))
              (else
               (assertion-violation 'config-fold "impossible!")))))
       config))

(define (config-fold kons nil cfg)
  (define (lose msg . irritants)
    (apply error 'config-fold msg irritants))
  (unless (file-readable? cfg)
    (lose "file not readable" cfg))
  (fold (lambda (prj rest)
          (kons (car prj) (pathname-as-directory (cadr prj)) (caddr prj) rest))
        nil
        (normalize-config (call-with-input-file cfg read))))

(define (config-for-each proc cfg)
  (config-fold (lambda (rcs dir repo rest) (proc rcs dir repo) rest) (unspecific) cfg))

(define (build-config cfg . args)
  (let-optionals* args ((mode 'ask))
    (config-for-each
     (lambda (rcs dir repo)
       (if (file-exists? dir)
           (let ((pull
                  (lambda ()
                    (with-working-directory dir (rcs/pull rcs repo))))
                 (fresh
                  (lambda ()
                    (let ((tmp (temp-name dir)))
                      (rename-file dir tmp)
                      (rcs/get rcs repo dir))))
                 (push
                  (lambda ()
                    (with-working-directory dir
                      (rcs/push rcs repo)))))
             (case mode
               ((ask)
                (choose (string-append dir " exists: ")
                        #f
                        `((,(string-append "Update pulling from "
                                           repo)
                           ,pull)
                          ("Get a fresh copy" ,fresh))))
               ((pull) (pull))
               ((fresh) (fresh))
               ((push)  (push))))
           (rcs/get rcs repo dir)))
     cfg)))

(define (config-inventory cfg)
  (config-fold
   (lambda (rcs dir repo rest)
     (let ((file-list (with-working-directory dir
                        (map (lambda (filename)
                               (pathname-join dir filename))
                             (rcs/inventory rcs)))))
       (append rest (if (empty-pathname? dir)
                        file-list
                        (cons dir file-list)))))
   '()
   cfg))

(define (config-diff cfg . args)
  (reverse
   (config-fold
    (lambda (rcs dir repo rest)
      (with-working-directory dir
        (cons (cons dir (rcs/diff rcs)) rest)))
    '()
    cfg)))


(define (sys-def-extractor key)
  (lambda (form)
    (match form
      (('define-system name clauses ___)
       (append-map (lambda (clause)
                     (if (eq? (car clause) key)
                         (cdr clause)
                         '()))
                   clauses))
      (_
       '()))))

(define (config-extra-dist cfg)
  (config-fold
   (lambda (rcs dir repo extra-dist)
     (let ((sys-def (pathname-join dir "sys-def.scm")))
       (append
        (map (lambda (filename)
               (pathname-join dir filename))
             (if (file-exists? sys-def)
                 (append-map (sys-def-extractor 'extra-dist)
                             (call-with-input-file (x->namestring sys-def)
                               port->sexps))
                 '()))
        extra-dist)))
   '()
   cfg))

(define (config-dist cfg . args)
  (let ((dot-pos (string-index-right cfg #\.))
        (slash-pos (or (string-index-right cfg #\/) -1)))
    (let-optionals* args
        ((name (substring/shared cfg
                                 (+ slash-pos 1)
                                 (or (and dot-pos (> dot-pos slash-pos) dot-pos)
                                     (string-length cfg)))))
      (let ((dirname (pathname-as-directory name)))
        (create-directory dirname)
        (for-each (lambda (filename)
                    (let ((dst-name (pathname-join dirname filename)))
                      (if (file-directory? filename)
                          (create-directory* dst-name)
                          (create-hard-link filename dst-name))))
                  (append (config-inventory cfg)
                          (config-extra-dist cfg)))
        (run-process #f "tar" "-czf" (string-append name ".tar.gz") name)
        (run-process #f "rm" "-rf" name)))))

(define (port->sexps port)
  (unfold eof-object? values (lambda (seed) (read port)) (read port)))
