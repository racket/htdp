
(module imageeq mzscheme
  
  (provide image? image=?)

  (define (image=? a b)
    (let ([image=? (with-handlers ([not-break-exn? (lambda (x) #f)])
		     (dynamic-require '(lib "imageeq.ss" "lang" "private") 'image=?))])
      (if image=?
	  (image=? a b)
	  (raise-type-error 'image=? "image" 0 a b))))

  (define (image? a)
    (let ([image? (with-handlers ([not-break-exn? (lambda (x) #f)])
		    (dynamic-require '(lib "imageeq.ss" "lang" "private") 'image?))])
      (if image?
	  (image? a)
	  #f))))

