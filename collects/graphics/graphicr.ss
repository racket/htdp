(compound-unit/sig
  (import [f : mzlib:file^]
          [mred : mred^])
  (link [p : graphics:posn^ ((unit/sig graphics:posn^ (import) (define-struct posn (x y))))]
        [g : graphics:posn-less^
           ((require-relative-library "graphicspr.ss") f mred p)])
  (export
   (open p)
   (open g)))