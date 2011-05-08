;;; base.sls --- Conjure base library

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

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
  (export <step> <task> <project>
          <ordinary-task> <ordinary-step>
          <file-step> <file-task>
          register-task-prototype
          find-task-prototype
          logger:conjure)
  (import (rnrs base)
          (rnrs control)
          (only (rnrs lists) assq remp)
          (rnrs hashtables)
          (rnrs mutable-pairs)
          (except (srfi :1 lists) for-each map)
          (srfi :8 receive)
          (srfi :19 time)
          (only (spells misc) and=> topological-sort unspecific)
          (spells define-values)
          (spells alist)
          (spells opt-args)
          (spells match)
          (spells pathname)
          (spells filesys)
          (spells process)
          (spells logging)
          (spells tracing)
          (conjure utils)
          (conjure cmd-line)
          (wak foof-loop)
          (wak prometheus)
          (wak fmt))

;;; Step

(define-object <step> (*the-root-object*)
  (prerequisites '())
  (stale? #t)

  ((stale-rec? self resend)
   (or (self 'stale?)
       (find (lambda (t) (t 'stale-rec?)) (self 'prerequisites))))
  
  ((build-rec self resend . maybe-force?)
   (let ((force? (:optional maybe-force? #f)))
     (send-all (if force?
                   (dependency-list self)
                   (stale-dependency-list self))
               'build)))
  
  ((build self resend)
   (self 'add-value-slot! 'stale? #f))
  
  ((clean self resend)
   (unspecific))

  ((prop self resend property)
   ((self 'task) 'prop property))

  ((name self resend)
   ((self 'task) 'name))
  
  ((invoke self resend action targets)
   (unless (null? targets)
     (raise-task-error 'task "step does not have sub-targets"))
   (self action))
  
  ((dsp self resend)
   (cat "[step " ((self 'task) 'name)
        " [" (fmt-join dsp-obj (self 'prerequisites) " ") "]]")))

(define (dependency-dag step)
  (let ((deps (step 'prerequisites)))
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
                 (not (null? (lset-intersection eq?
                                                sdl
                                                ((car dl) 'prerequisites)))))
             (loop (cdr dl) (cons (car dl) sdl)))
            (else
             (loop (cdr dl) sdl))))))

;;; Task

(define-object <task> (*the-root-object*)
  (name %set-name! #f)
  (step-prototype <step>)
  
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
                       ((name ('virtual type) setter) #f)
                       ((name type default)    name)
                       (else                   #f)))
                   info))
     (define (singleton-prop? name)
       (cond ((assq name info) => (lambda (propinfo)
                                    (match (propinfo-type propinfo)
                                      (('singleton type) #t)
                                      (_                 #f))))
             (else #f)))
     (define (combine-args+props)
       (let loop ((props props) (arg-info arg-info) (args args))
         (cond ((null? args)
                props)
               ((null? arg-info)
                (raise-task-error 'task "superfluous arguments" args))
               (else
                (let ((arg-val (if (singleton-prop? (car arg-info))
                                   (list (car args))
                                   (car args))))
                  (loop (cons (cons (car arg-info) arg-val) props)
                        (cdr arg-info)
                        (cdr args)))))))
     (t '%set-name! name)
     (let loop ((specified '()) (virtuals '()) (ps (combine-args+props)))
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
                                     (coerce default
                                             (propinfo-type propinfo)))))))))
              defaulted)
             (for-each (lambda (proc) (proc)) virtuals)
             t)
           (let ((propname (caar ps))
                 (propval (cdar ps)))
             (cond ((assq propname info)
                    => (lambda (propinfo)
                         (loop
                          (cons propname specified)
                          (cond
                           ((match propinfo
                              ((name 'virtual setter)
                               (lambda () (setter t propval)))
                              ((name ('virtual type) setter)
                               (lambda ()
                                 (setter t (coerce propval type))))
                              ((name type default-opt ___)
                               (t 'set-prop! propname (coerce propval type))
                               #f))
                            => (lambda (v) (cons v virtuals)))
                           (else
                            virtuals))
                          (cdr ps))))
                   (else
                    (raise-task-error 'task "no such property" propname))))))))

  (arguments '())
  (properties '())

  (%props %set-props! '())

  ((prop self resend name)
   (cond ((assq name (self '%props)) => cdr)
         (else (assertion-violation '<task>/prop "no such property" name))))

  ((set-prop! self resend name value)
   (cond ((assq name (self '%props))
          => (lambda (prop)
               (set-cdr! prop value)))
         (else
          (self '%set-props! (cons (cons name value) (self '%props))))))

  ((dependencies self resend) '())
  ((sources self resend) '())
  ((products self resend) '())

  ((construct-step self resend project)
   (object ((self 'step-prototype))
     (task self)
     (prerequisites (map (lambda (dep) (project 'get-step dep))
                         (self 'dependencies)))
     (project project)))

  ((disclose self resend)
   `(task ,(self 'name) (props ,@(self '%props))))

  ((wrt self resend)
   (wrt (self 'disclose)))

  ((dsp self resend)
   (cat "[task " (self 'name) "]"))

  )

;;; Project

(define-object <project-step> (<step>)
  ((clean self resend)
   ((self 'task) 'clean))
  
  ((build self resend)
   (resend #f 'build)
   (log/project 'info (cat "building " (self 'name)))
   ((self 'task) 'build-rec))

  ((invoke self resend action targets)
   ((self 'task) 'invoke action targets)))

(define-object <project> (<task>)
  (arguments '(product-dir source-dir))
  (properties `((source-dir (virtual (singleton pathname))
                            ,(vprop-setter/dir 'set-source-dir!))
                (product-dir (virtual (singleton pathname))
                             ,(vprop-setter/dir 'set-product-dir!))
                (default-task (singleton any) (#f))))

  ;; Value slots
  (product-dir set-product-dir! (make-pathname #f '() #f))
  (source-dir set-source-dir! (make-pathname #f '() #f))
  (named-tasks set-named-tasks! #f)
  (task-steps set-task-steps! #f)
  (product-tasks set-product-tasks! #f)
  (tasks set-tasks! '())
  (rules set-rules! '())
  (%first-task %set-first-task! #f)
  (step-prototype <project-step>)
  
  ;; Constructor
  ((new self resend name args props)
   (let ((proj (resend #f 'new name args props)))
     (proj 'set-source-dir! (enough-pathname (proj 'source-dir) (proj 'product-dir)))
     (proj 'set-named-tasks! (make-eq-hashtable))
     (proj 'set-task-steps! (make-eq-hashtable))
     (proj 'set-product-tasks! (make-hashtable pathname-hash pathname=?))
     (log/project 'debug (cat (dsp-obj proj) " created"))
     proj))

  ((add-task self resend task)
   (let ((task-name (task 'name))
         (prod-tasks (self 'product-tasks)))
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
     (when task-name
       (hashtable-set! (self 'named-tasks) task-name task))
     (unless (self '%first-task)
       (self '%set-first-task! task))
     (self 'set-tasks! (cons task (self 'tasks)))
     (log/project 'debug (cat (self 'name) ": added " (dsp-obj task)))))

  ((build-rec self resend . maybe-force?)
   (let ((force? (:optional maybe-force? #f)))
     (with-project-product-dir self
       (lambda ()
         (cond ((or (and=> (self 'prop 'default-task)
                           (lambda (default)
                             (self 'get-task default)))
                    (self '%first-task))
                => (lambda (default-task)
                     ((self 'get-step default-task) 'build-rec force?))))))))

  ((clean self resend)
   (with-project-product-dir self
     (lambda () (send-all (project-steps self) 'clean))))

  ((has-product? self resend name)
   ;; TODO: rules?
   (and (hashtable-ref (self 'product-tasks) name #f) #t))

  ((get-step self resend name/product . maybe-fallback)
   (let ((task-steps (self 'task-steps))
         (task (if (procedure? name/product)
                   name/product
                   (apply self 'get-task name/product maybe-fallback))))
     (and task
          (cond ((hashtable-ref task-steps task #f)
                 => values)
                (else
                 (let ((step (task 'construct-step self)))
                   (log/project 'debug (cat "constructed " (dsp-obj step)
                                            " from " (dsp-obj task)))
                   (hashtable-set! task-steps task step)
                   step))))))

  ((get-task self resend name/product . maybe-fallback)
   (let ((fallback (:optional maybe-fallback
                              (lambda ()
                                (build-failure "no step found for building {0}"
                                               name/product)))))
     (cond ((symbol? name/product)
            (or (hashtable-ref (self 'named-tasks) name/product #f)
                (fallback)))
           ((pathname? name/product)
            (or (hashtable-ref (self 'product-tasks) name/product #f)
                ;; TODO: rules would go here
                (fallback)))
           (else
            (assertion-violation '<project>/get-task
                                 "invalid name/product" name/product)))))

  ((invoke self resend action targets)
   (if (null? targets)
       (self action)
       (with-project-product-dir self
         (lambda () (invoke-project-targets self action targets)))))

  ((dsp self resend)
   (cat "[project " (self 'name) " "
        (dsp-pathname (self 'product-dir)) " <= "
        (dsp-pathname (self 'source-dir))
        "]"))

  )

(define (invoke-project-targets project action targets)
  (loop ((for target (in-list targets)))
    (match target
      ((target . sub-targets)
       (let ((step (project 'get-step (string->symbol target))))
         (step 'invoke action sub-targets)))
      (target
       ((project 'get-step (string->symbol target)) action)))))

(define (root-steps proj)
  (filter-map (lambda (adj)
                (and (null? (cdr adj)) (car adj)))
              (precondition-dag proj)))

(define (precondition-dag proj)
  (invert-dag (project-dependency-dag proj)))

(define (project-steps project)
  (map (lambda (task) (project 'get-step task)) (project 'tasks)))

(define (project-dependency-dag proj)
  (map (lambda (step)
         (cons step (step 'prerequisites)))
       (project-steps proj)))

(define (with-project-product-dir project thunk)
  (let ((product-dir (project 'product-dir)))
    (cond ((pathname=? product-dir (make-pathname #f '() #f))
           (thunk))
          (else
           (unless (file-exists? product-dir)
             (create-directory* product-dir))
           (log/project 'info (cat "entering " (dsp-pathname product-dir)))
           (with-working-directory product-dir thunk)
           (log/project 'info (cat "leaving " (dsp-pathname product-dir)))))))

;;; Ordinary task

(define-object <ordinary-step> (<step>)
  ((build self resend)
   (for-each (lambda (cmd)
                 (run-shell-command cmd))
               (self 'prop 'system))
   (for-each (lambda (proc)
               (proc self))
             (self 'prop 'proc))
   (resend #f 'build)))

(define-object <ordinary-task> (<task>)
  (arguments '(product))
  (properties
   `((product (virtual (singleton pathname)) ,(vprop-setter 'set-products! list))
     (products (virtual (list-of pathname)) ,(vprop-setter 'set-products!))
     (sources (virtual (list-of pathname)) ,(vprop-setter 'set-sources!))
     (depends (virtual (list-of symbol)) ,(vprop-setter 'set-dependencies!))
     (system (list-of string) ())
     (proc (list-of procedure) ())))
  (step-prototype <ordinary-step>)
  (products set-products! '())
  (sources set-sources! '())
  (dependencies set-dependencies! '()))

;;; File-based task

(define-object <file-step> (<ordinary-step>)
  ((clean self resend)
   (for-each delete-file (self 'products)))
  
  ((stale? self resend)
   (let* ((missing-products (remp file-exists? (self 'products)))
          (missing-sources (remp file-exists? (self 'sources)))
          (source-lmt (and (null? missing-sources)
                           (last-modification-time (self 'sources))))
          (product-lmt (and (null? missing-products)
                            (last-modification-time (self 'products)))))
     (define (dsp-staleness)
       (lambda (st)
         (cond ((not (null? missing-products))
                ((cat (dsp-obj self) " is stale; missing products: "
                      (fmt-join dsp-pathname missing-products " ")) st))
               ((not (null? missing-sources))
                ((cat (dsp-obj self) " is stale; missing sources: "
                      (fmt-join dsp-pathname missing-sources)) st))
               ((and source-lmt product-lmt (time>? source-lmt product-lmt))
                ((cat (dsp-obj self) " is stale; products older ("
                      (dsp-time-utc product-lmt) ") than sources ("
                      (dsp-time-utc source-lmt) ")") st))
               (else
                ((cat (dsp-obj self) " is up to date") st)))))

     (log/task 'debug (dsp-staleness))
     (or (not (null? missing-products))
         (not (null? missing-sources))
         (and source-lmt product-lmt (time>? source-lmt product-lmt)))))

  ((resolve-files self resend files)
   (map (lambda (file)
          (self 'resolve-file file))
        files))

  ((resolve-file self resend file)
   (let ((project (self 'project)))
     (pathname-join (if (project 'has-product? file)
                        '(())
                        (project 'source-dir))
                    file)))
  
  ((dsp self resend)
   (cat "[file-task " (fmt-join dsp-pathname (self 'products) " ")
        " <= " (fmt-join dsp-pathname (self 'sources) " ") "]")))

(define-object <file-task> (<ordinary-task>)
  ((construct-step self resend project)
   (define (file-check source)
     (lambda ()
       (let ((filename (pathname-join (project 'source-dir) source)))
         (cond ((file-exists? filename)
                #f)
               (else
                (build-failure "no step found for building {0}"
                               (x->namestring filename)))))))
   (let* ((step (resend #f 'construct-step project))
          (sources (step 'resolve-files (self 'sources)))
          (products (self 'products)))
     (modify-object!
      step
      (prerequisites (append (step 'prerequisites)
                             (filter-map
                              (lambda (source)
                                (project 'get-step source (file-check source)))
                              (self 'sources))))
      (sources sources)
      (products products))
     step)))

;;; Task registry

(define-values (find-task-prototype register-task-prototype)
  (let ((prototypes '()))
    (values
     (lambda (name)
       (assq-ref prototypes name))
     (lambda (name prototype)
       (cond ((assq name prototypes)
              => (lambda (entry)
                   (set-cdr! entry prototype)))
             (else
              (set! prototypes (cons (cons name prototype) prototypes))))))))

;; Loggers and log procedures

(define logger:conjure (make-logger root-logger 'conjure))
(define logger:conjure.project (make-logger logger:conjure 'project))
(define logger:conjure.task (make-logger logger:conjure 'task))

(define log/project (make-fmt-log logger:conjure.project))
(define log/task (make-fmt-log logger:conjure.task))

)

;; Local Variables:
;; scheme-indent-styles: ((object 1) foof-loop)
;; End:
