
(reference-library "turtles.ss" "graphics")
(reference-library "functiou.ss")

(invoke-open-unit/sig 
 (compound-unit/sig
  (import)
  (link [T : turtle^ ((reference-library "turtler.ss" "graphics") F)]
	[F : mzlib:function^ (mzlib:function@)])
  (export (open T))))

(define-macro split 
  (lambda Es
    `(splitfn (lambda () (begin ,@Es)))))

(define-macro split* 
  (lambda Es 
    `(split*fn (list ,@(map (lambda (x) `(lambda () ,x)) Es)))))

(define-macro tprompt
  (lambda Es
    `(fluid-let ([Turtles Turtles]
		 [Cache Cache])
		,@Es)))

(define-macro repeat
  (match-lambda* 
   [((var n) . Es) (let ((loop (gensym 'repeat)))
		     `(let ,loop ((,var 1))
			(when (<= ,var ,n)
			  ,@Es
			  (,loop (add1 ,var)))))]
   [_ (error 'repeat "bad syntax")]))

