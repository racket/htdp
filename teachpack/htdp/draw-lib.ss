;; simple functions to get figures drawn (test)

(reference-file "error-lib.ss")
(reference-file "big-draw-lib.ss")

(compound-unit/sig
  (import (PLT : plt:userspace^))
  (link
    (DRAW : drawS (bigDrawU ERR PLT))
    (ERR  : errorS (errorU)))
  (export (open DRAW)))
