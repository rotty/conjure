;;; dsl.sls --- Conjure DSL

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

(library (conjure dsl)
  (export task
          project
          define-project
          with-project
          current-project
          import-procedures/lazy)
  (import (rnrs base)
          (rnrs control)
          (rnrs syntax-case)
          (rnrs io simple)
          (rnrs eval)
          (srfi :39 parameters)
          ;; (for (spells tracing) run expand)
          ;; (for (only (spells assert) cout) run expand)
          (spells define-values)
          (for (conjure utils) run expand)
          (conjure base))

(define-syntax task
  (lambda (stx)
    (syntax-case stx ()
      ((_ (type props ...))
       #'(task #f (type props ...)))
      ((_ name (type prop ...))
       (let-values (((args props) (split-props #'(prop ...))))
         (with-syntax (((arg ...) args)
                       ((prop ...) props))
           #'(add-task 'type 'name
                       (list arg ...)
                       (list prop ...))))))))

(define-syntax project
  (lambda (stx)
    (syntax-case stx ()
      ((_ name (prop ...) body ...)
       (let-values (((args props) (split-props #'(prop ...))))
         (with-syntax (((arg ...) args)
                       ((prop ...) props))
           #'(parameterize ((current-project
                             (<project> 'new 'name
                                        (list arg ...)
                                        (list prop ...))))
               body ...
               (current-project))))))))

(define-syntax define-project
  (syntax-rules ()
    ((_ name (prop ...) body ...)
     (define name (project name (prop ...) body ...)))))

(define (with-project project thunk)
  (parameterize ((current-project project))
    (thunk)))

(define current-project (make-parameter #f))

(define (add-task type name args props)
  (let ((prototype (find-task-prototype type)))
    (unless prototype
      (error 'add-task "no prototype with that name found" type))
    ((current-project) 'add-task (prototype 'new name args props))))


(define-syntax import-procedures/lazy
  (syntax-rules (only)
    ((_ (only import proc-id ...) ...)
     (begin
       (define-values (proc-id ...)
         (let ((env #f)
               (proc-id #f)
               ...)
           (define (force-environment)
             (unless env
               (set! env (environment 'import)))
             env)
           (values (lambda args
                     (unless proc-id
                       (set! proc-id (eval 'proc-id (force-environment))))
                     (apply proc-id args))
                   ...)))
       ...))))

)
