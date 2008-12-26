#!r6rs

(library (rcs42 prompt)
  (export y-or-n choose)
  (import (rnrs)
          (spells include))
  
  (include-file ((rcs42 scheme) prompt)))
