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
  (export <step> <task> <project>)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs hashtables)
          (rnrs mutable-pairs)
          (srfi :1 lists)
          (only (spells misc) topological-sort)
          (spells opt-args)
          (spells match)
          (spells pathname)
          (spells filesys)
          (only (spells assert) cout)
          (spells tracing)
          (conjure utils)
          (prometheus))

;;; Step

(define-object <step> (*the-root-object*)
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

(define (dependency-dag step)
  (let ((deps (step 'dependencies)))
    (if (null? deps)
        (list (list step))
        (concatenate (cons (list (cons step deps))
                           (map dependency-dag deps))))))

(define (dependency-list step)
  (reverse (topological-sort (dependency-dag step))))

(define (stale-dependency-list step)
  (let ((dl (dependency-list step)))
    (let loop ((dl dl) (sdl '()))
      (cond ((null? dl)
             (reverse sdl))
            ((or ((car dl) 'stale?)
                 (not (null? (lset-intersection eq? sdl ((car dl) 'dependencies)))))
             (loop (cdr dl) (cons (car dl) sdl)))
            (else
             (loop (cdr dl) sdl))))))

;;; Task

(define-object <task> (*the-root-object*)
  (name %set-name! #f)

  ((new self resend name args props)
   (let* ((t (self 'clone))
          (info (t 'properties))
          (arg-info (t 'arguments)))
     (define (required-props)
       (filter-map (lambda (propinfo)
                     (and (null? (cddr propinfo)) (car propinfo)))
                   info))
     (define (defaulted-props)
       (filter-map (lambda (propinfo)
                     (match propinfo
                       ((name 'virtual setter) #f)
                       ((name type default)    name)
                       (else                   #f)))
                   info))
     (define (combine-args+props)
       (let loop ((props props) (arg-info arg-info) (args args))
         (cond ((null? args)
                props)
               ((null? arg-info)
                (raise-task-error 'task "superfluous arguments" args))
               (else
                (loop (cons (cons (car arg-info) (car args)) props)
                      (cdr arg-info)
                      (cdr args))))))
     (t '%set-name! name)
     (let loop ((specified '()) (ps (combine-args+props)))
       (if (null? ps)
           (let ((missing (lset-difference eq? (required-props) specified))
                 (defaulted (lset-difference eq? (defaulted-props) specified)))
             (unless (null? missing)
               (raise-task-error 'task "missing properties" missing))
             (for-each
              (lambda (propname)
                (cond ((assq propname info)
                       => (lambda (propinfo)
                            (let ((default (caddr propinfo)))
                              (t 'set-prop! propname
                                 (if (procedure? default)
                                     (default t)
                                     default)))))))
              defaulted)
             t)
           (let ((propname (caar ps))
                 (propval (cdar ps)))
             (cond ((assq propname info)
                    => (lambda (propinfo)
                         (case (cadr propinfo)
                           ((virtual)
                            ((caddr propinfo) t propval))
                           (else
                            (t 'set-prop! propname
                               (coerce propval (cadr propinfo)))))
                         (loop (cons propname specified)
                               (cdr ps))))
                   (else
                    (raise-task-error 'task "no such property" propname))))))))

  (arguments '())
  (properties '())

  (%props %set-props! '())

  ((prop self resend name)
   (cond ((assq name (self '%props)) => cdr)
         (else (error '<task>.prop "no such property" name))))

  ((set-prop! self resend name value)
   (cond ((assq name (self '%props))
          => (lambda (prop)
               (set-cdr! prop value)))
         (else
          (self '%set-props! (cons (cons name value) (self '%props))))))

  ((dependencies self resend) '())
  ((sources self resend) '())
  ((products self resend) '()))

;;; Project

(define-object <project> (<task>)
  (arguments '(source-dir product-dir))
  (properties `((source-dir virtual
                            ,(vprop-setter 'set-source-dir! x->pathname))
                (product-dir virtual
                             ,(vprop-setter 'set-product-dir! x->pathname))))

  ;; Value slots
  (product-dir set-product-dir! (make-pathname #f '() #f))
  (source-dir set-source-dir! (make-pathname #f '() #f))
  (named-tasks set-named-tasks! #f)
  (task-steps set-task-steps! #f)
  (product-tasks set-product-tasks! #f)
  (tasks set-tasks! '())
  (rules set-rules! '())

  ;; Constructor
  ((new self resend name src-dir prod-dir)
   (let ((reg (resend #f 'new name '() '())))
     (reg 'set-source-dir! (x->pathname src-dir))
     (reg 'set-product-dir! (x->pathname prod-dir))
     (reg 'set-named-tasks! (make-eq-hashtable))
     (reg 'set-task-steps! (make-eq-hashtable))
     (reg 'set-product-tasks! (make-hashtable string-hash string=?))
     reg))

  ((add-task self resend task)
   (let ((task-name (task 'name))
         (prod-tasks (self 'product-tasks)))
     (when task-name
       (hashtable-set! (self 'named-tasks) task))
     (for-each
      (lambda (prod)
        (hashtable-update!
         prod-tasks
         prod
         (lambda (prev)
           (when prev
             (build-failure "already have a task yielding this product"
                            prev prod))
           task)
         #f))
      (task 'products))
     (self 'set-tasks! (cons task (self 'tasks)))))

  ((build-rec self resend . maybe-force?)
   (let ((force? (*optional maybe-force? #f)))
     (unless (file-exists? (self 'product-dir))
       (create-directory (self 'product-dir)))
     (send-all (root-steps self) 'build-rec force?)))

  ((clean self resend)
   (send-all (root-steps self) 'clean)
   (delete-file (self 'product-dir)))

  ((has-product? self resend name)
   ;; TODO: rules?
   (and (hashtable-ref (self 'product-tasks) name #f) #t))

  ((get-step self resend name/product . maybe-fallback)
   (let* ((fallback (*optional maybe-fallback
                               (lambda ()
                                 (build-failure "no step found for building {0}"
                                                name/product))))
          (task-steps (self 'task-steps))
          (task
           (cond ((symbol? name/product)
                  (or (hashtable-ref (self 'named-tasks) name/product #f)
                      (fallback)))
                 ((string? name/product)
                  (or (hashtable-ref (self 'product-tasks) name/product #f)
                      ;; TODO: rules would go here
                      (fallback)))
                 (else
                  (error '<project>/get-step
                         "invalid name/product" name/product)))))
     (and task
          (cond ((hashtable-ref task-steps task #f)
                 => values)
                (else
                 (let ((step (task 'construct-step self)))
                   (hashtable-set! task-steps task step)
                   step)))))))

(define (root-steps reg)
  (filter-map (lambda (adj)
                (and (null? (cdr adj)) (car adj)))
              (precondition-dag reg)))

(define (precondition-dag reg)
  (invert-dag (project-dependency-dag reg)))

(define (project-steps reg)
  (let ((task-steps (reg 'task-steps)))
    (let loop ((tasks (reg 'tasks)) (steps '()))
      (cond ((null? tasks)
             (reverse steps))
            ((hashtable-ref task-steps (car tasks) #f)
             => (lambda (step)
                  (loop (cdr tasks) (cons step steps))))
            (else
             (let* ((task (car tasks))
                    (step (task 'construct-step reg)))
               (hashtable-set! task-steps task step)
               (loop (cdr tasks) (cons step steps))))))))

(define (project-dependency-dag reg)
  (map (lambda (step)
         (cons step (step 'dependencies)))
       (project-steps reg)))

)
