
(require-library "turtles.ss" "graphics")
(require-library "functiou.ss")

(invoke-open-unit/sig 
 (compound-unit/sig
  (import)
  (link [t : turtle^ ((require-library "turtler.ss" "graphics") f)]
	[f : mzlib:function^ (mzlib:function@)])
  (export (open t))))

(require-relative-library "tmacro.ss")

