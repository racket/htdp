(require-library "error.ss" "htdp")
(reference-file "big-draw.ss")

(compound-unit/sig
  (import (PLT : plt:userspace^))
  (link
    (DRAW : drawS (bigDrawU ERR PLT))
    (ERR  : errorS (errorU)))
  (export (open DRAW)))
