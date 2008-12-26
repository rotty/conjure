;; -*- mode: scheme; scheme48-package: (config); -*-

(define-structure rcs42.utils (export and-map or-map
                                      identity
                                      unspecific
                                      make-reinitializer
                                      let-optionals*)
  (open scheme srfi-23
        (subset primitives (unspecific))
        define-record-types
        record-types)
  (files utils))

(define-structure rcs42.rdelim (export read-line)
  (open scheme ports (subset primitives (eof-object))
        srfi-8
        srfi-13
        srfi-23
	(subset srfi-14 (char-set x->char-set char-set-contains?))
	byte-vectors
        ascii
        posix-regexps
        rcs42.utils)
  (files rdelim))

(define-structure rcs42.prompt (export y-or-n choose)
  (open scheme)
  (files prompt))

(define-structure rcs42.files (export file->string-list
                                      temp-name
                                      create-temp-directory
                                      (with-cwd :syntax)
                                      normalize-filename)
  (open scheme srfi-1 srfi-8 srfi-13 srfi-23 srfi-34 srfi-36
        fluids
        formats
        posix-files
        posix-process-data
        rcs42.rdelim
        rcs42.utils)
  (files files scsh-files))

(define-structure rcs42.darcs (export pull
                                      push
                                      run-process/lines
                                      build-config
                                      inventory
                                      config-inventory
                                      config-whatsnew
                                      config-dist)
  (open scheme srfi-1 srfi-8 srfi-13 srfi-23
        (subset features (force-output))
        posix-regexps
        posix-files
        lib42.process
        rcs42.prompt rcs42.files rcs42.rdelim rcs42.utils)
  (files darcs))

(define-structure rcs42.tools (export whatsnew push inventory dist pull)
  (open scheme srfi-8 srfi-23
        rcs42.darcs)
  (files tools))
