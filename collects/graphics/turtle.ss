
(reference-library "turtles.ss" "graphics")
(reference-library "functiou.ss")

(invoke-open-unit/sig 
 (compound-unit/sig
  (import)
  (link [T : turtle^ ((reference-library "turtler.ss" "graphics") F)]
	[F : mzlib:function^ (mzlib:function@)])
  (export (open T))))

(require-relative-library "tmacro.ss")

