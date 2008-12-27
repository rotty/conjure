#!r6rs

(library (rcs42 darcs)
  (export pull
          push
          run-process/lines
          build-config
          inventory
          config-inventory
          config-whatsnew
          config-dist)
  
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs control)
          (rnrs io simple)
          (rnrs io ports)
          (spells lists)
          (spells receive)
          (spells strings)
          (spells pathname)
          (spells filesys)
          (spells process)
          (spells delimited-readers)
          (spells misc)
          (spells opt-args)
          (spells tracing)
          (spells include)
          (xitomatl irregex)
          (rcs42 files)
          (rcs42 prompt))
  
  (include-file ((rcs42 scheme) darcs)))
