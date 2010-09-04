(package (conjure (0 1 0))
  (depends (srfi)
           (wak-fmt)
           (wak-irregex)
           (wak-foof-loop)
           (wak-prometheus)
           (spells))
  
  (synopsis "Scheme make replacement")
  (description
   "conjure is a library, including a thin domain specific"
   "language layer, that allows expressing build instructions"
   "such as traditionally found in Makefiles.")
  (homepage "http://github.com/rotty/conjure")
  
  (libraries
   (sls -> "conjure")))

;; Local Variables:
;; scheme-indent-styles: ((package 1))
;; End:
