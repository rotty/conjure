#!r6rs

(library (rcs42 files)
  (export temp-name
          create-temp-directory)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs io ports)
          (srfi :8 receive)
          (srfi :39 parameters)
          (spells format)
          (spells delimited-readers)
          (spells filesys)
          (spells sysutils)
          (spells include))
  
  (include-file ((rcs42 private) files))
  (include-file ((rcs42 private) scsh-files)))
