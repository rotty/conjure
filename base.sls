;;; base.sls --- Conjure base library

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
(library (conjure base)
  (export <task> <goal> <register>)
  (import (rnrs base)
          (rnrs control)
          (rnrs hashtables)
          (srfi :1 lists)
          (only (spells misc) topological-sort)
          (spells opt-args)
          (spells pathname)
          (spells filesys)
          (conjure utils)
          (prometheus))

;;; Task

(define-object <task> (*the-root-object*)
  ((dependencies self resend) '())
  ((stale? self resend) #f)
  ((stale-rec? self resend)
   (or (self 'stale?)
       (find (lambda (t) (t 'stale-rec?)) (self 'dependencies))))
  ((build-rec self resend . maybe-force?)
   (let* ((force? (*optional maybe-force? #f))
          (deps (if force?
                    (dependency-list self)
                    (stale-dependency-list self))))
     (send-all deps 'build))))

(define (dependency-dag task)
  (let ((deps (task 'dependencies)))
    (if (null? deps)
        (list (list task))
        (concatenate (cons (list (cons task deps))
                           (map dependency-dag deps))))))

(define (dependency-list task)
  (reverse (topological-sort (dependency-dag task))))

(define (stale-dependency-list task)
  (let ((dl (dependency-list task)))
    (let loop ((dl dl) (sdl '()))
      (cond ((null? dl)
             (reverse sdl))
            ((or ((car dl) 'stale?)
                 (not (null? (lset-intersection eq? sdl ((car dl) 'dependencies)))))
             (loop (cdr dl) (cons (car dl) sdl)))
            (else
             (loop (cdr dl) sdl))))))

;;; Goal

(define-object <goal> (*the-root-object*)
  (name set-name! #f)
  ((dependencies self resend) '())
  ((sources self resend) '())
  ((products self resend) '()))

;;; Register

(define-object <register> (<goal>)
  ;; Value slots
  (product-dir set-product-dir! (make-pathname #f '() #f))
  (source-dir set-source-dir! (make-pathname #f '() #f))
  (named-goals set-named-goals! #f)
  (goal-tasks set-goal-tasks! #f)
  (product-goals set-product-goals! #f)
  (goals set-goals! '())
  (rules set-rules! '())

  ;; Constructor
  ((new self resend name src-dir prod-dir)
   (let ((reg (self 'clone)))
     (reg 'set-name! name)
     (reg 'set-source-dir! (x->pathname src-dir))
     (reg 'set-product-dir! (x->pathname prod-dir))
     (reg 'set-named-goals! (make-eq-hashtable))
     (reg 'set-goal-tasks! (make-eq-hashtable))
     (reg 'set-product-goals! (make-hashtable string-hash string=?))
     reg))

  ((add-goal self resend goal)
   (let ((goal-name (goal 'name))
         (prod-goals (self 'product-goals)))
     (when goal-name
       (hashtable-set! (self 'named-goals) goal))
     (for-each
      (lambda (prod)
        (hashtable-update!
         prod-goals
         prod
         (lambda (prev)
           (when prev
             (build-failure "already have a goal yielding this product"
                            prev prod))
           goal)
         #f))
      (goal 'products))
     (self 'set-goals! (cons goal (self 'goals)))))

  ((build-rec self resend . maybe-force?)
   (let ((force? (*optional maybe-force? #f)))
     (unless (file-exists? (self 'product-dir))
       (create-directory (self 'product-dir)))
     (send-all (root-tasks self) 'build-rec force?)))

  ((clean self resend)
   (send-all (root-tasks self) 'clean)
   (delete-file (self 'product-dir)))

  ((has-product? self resend name)
   ;; TODO: rules?
   (and (hashtable-ref (self 'product-goals) name #f) #t))

  ((get-task self resend name/product . maybe-fallback)
   (let* ((fallback (*optional maybe-fallback
                               (lambda ()
                                 (build-failure "no task found for building {0}"
                                                name/product))))
          (goal-tasks (self 'goal-tasks))
          (goal
           (cond ((symbol? name/product)
                  (or (hashtable-ref (self 'named-goals) name/product #f)
                      (fallback)))
                 ((string? name/product)
                  (or (hashtable-ref (self 'product-goals) name/product #f)
                      ;; TODO: rules would go here
                      (fallback)))
                 (else
                  (error '<register>/get-task
                         "invalid name/product" name/product)))))
     (and goal
          (cond ((hashtable-ref goal-tasks goal #f)
                 => values)
                (else
                 (let ((task (goal 'construct-task self)))
                   (hashtable-set! goal-tasks goal task)
                   task)))))))

(define (root-tasks reg)
  (filter-map (lambda (adj)
                (and (null? (cdr adj)) (car adj)))
              (precondition-dag reg)))

(define (precondition-dag reg)
  (invert-dag (register-dependency-dag reg)))

(define (register-tasks reg)
  (let ((goal-tasks (reg 'goal-tasks)))
    (let loop ((goals (reg 'goals)) (tasks '()))
      (cond ((null? goals)
             (reverse tasks))
            ((hashtable-ref goal-tasks (car goals) #f)
             => (lambda (task)
                  (loop (cdr goals) (cons task tasks))))
            (else
             (let* ((goal (car goals))
                    (task (goal 'construct-task reg)))
               (hashtable-set! goal-tasks goal task)
               (loop (cdr goals) (cons task tasks))))))))

(define (register-dependency-dag reg)
  (map (lambda (task)
         (cons task (task 'dependencies)))
       (register-tasks reg)))

)
