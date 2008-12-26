#!r6rs

(library (rcs42 files)
  (export temp-name
          create-temp-directory)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs io ports)
          (spells receive)
          (spells parameter)
          (spells format)
          (spells delimited-readers)
          (spells filesys)
          (spells sysutils)
          (spells include))
  
  (include-file ((rcs42 scheme) files))
  (include-file ((rcs42 scheme) scsh-files)))
