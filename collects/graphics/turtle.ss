(require-library "turtles.ss" "graphics")
(require-library "functiou.ss")
(require-library "invoke.ss")

(define-values/invoke-unit/sig
  turtle^
 (compound-unit/sig
  (import)
  (link [t : turtle^ ((require-library "turtler.ss" "graphics") f)]
	[f : mzlib:function^ (mzlib:function@)])
  (export (open t))))

(require-relative-library "tmacro.ss")

