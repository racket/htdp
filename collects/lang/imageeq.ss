
(module imageeq mzscheme
  
  (provide image? image=?)

  (define im=? 'unknown)

  (define (image=? a b)
    (unless (not im=?)
      (set! im=? (with-handlers ([not-break-exn? (lambda (x) #f)])
		   (dynamic-require '(lib "imageeq.ss" "lang" "private") 'image=?))))
    (if im=?
	(im=? a b)
	(raise-type-error 'image=? "image" 0 a b)))

  (define im? 'unknown)

  (define (image? a)
    (unless (not im?)
      (set! im? (with-handlers ([not-break-exn? (lambda (x) #f)])
		  (dynamic-require '(lib "imageeq.ss" "lang" "private") 'image?))))
    (if im?
	(im? a)
	#f)))

