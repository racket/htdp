(lambda (request failure)
  (let ([dr? (with-handlers 
	      ([exn:i/o:filesystem? 
		(lambda args #f)])
	      (collection-path "drscheme"))])
    (case request
      [(name) "stepper"]
      [(compile-prefix) 
       (if dr?
	   '(begin 
	      (require-library "sig.ss" "stepper")
	      (require-library "drsig.ss" "drscheme"))
	   '(begin
	      (require-library "sig.ss" "stepper")
	      (require-library "sigs.ss" "zodiac")
	      (require-library "cores.ss")))]
      [(compile-omit-files) 
       `("test.ss" "testr.ss" "sig.ss" 
	 ,@(if (not dr?)
	       '("break.ss" "view-controller.ss"
		 "model.ss" "instance.ss" "link.ss"
		 "reconstructr.ss" "startup.ss")
	       null))]
      [else (failure)])))
