;; TODO: docs, htdp update 
(define-signature convertS (convert-gui convert-repl convert-file))
(require-library "error.ss" "htdp")

(define convertU 
  (unit/sig convertS 
    (import errorS plt:userspace^)

   ;; scale% : (union false (num -> str)) frame% -> scale<%>
   ;; scale<%> : set-current-x + canvas<%>
   ;; the str produced by to-string shouldn't be more than 3 or 4 chars
   (define scale%
    (class* canvas% () (to-string . x)
      (sequence (apply super-init x))
      (inherit get-dc get-size get-client-size)    
      (private 
	;; managing the ratio
	(current-x  0)
	(check-x    
	  (lambda (x)
	    (if (and (number? x) (<= 0. x 1.))
		x 
		(error 'set-current-x "out of range [0.,1.]: ~e" x))))
	;; drawing things
	(dc         (send this get-dc))
	(black-pen  (make-object pen%   "BLACK" 2 'solid))
	(red-brush  (make-object brush% "RED"  'solid))
	(draw-something
	  (lambda (delta-x)
	    (send dc clear)
	    (let-values ([(width height) (get-size)])
	      (send dc set-brush red-brush)
	      (send dc draw-rectangle 0 0 delta-x height)
	      (send dc set-pen black-pen)
	      (let*-values ([(str)
			     (if to-string
				 (to-string current-x)
				 (string-append
				  "."
				  (number->string
				   (inexact->exact
				    (truncate
				     (exact->inexact (* 100 current-x)))))))]
			    [(cw ch) (get-client-size)]
			    [(sw sh _1 _2) (send dc get-text-extent str)])
		(send dc draw-text
		  str
		  (floor (- (/ cw 2) (/ (inexact->exact sw) 2)))
		  (floor (- (/ ch 2) (/ (inexact->exact sh) 2)))))))))
      (override
	[on-paint (lambda () 
		    (let-values ([(width height) (get-size)])
		      (draw-something (* current-x width))))])
      (public 
	[set-current-x (lambda (x) 
			 (set! current-x (check-x x))
			 (on-paint))])
      (inherit min-width min-height)
      (sequence
	(let-values ([(w h a d) (send (get-dc) get-text-extent "100")])
	  (min-width (inexact->exact w))
	  (min-height (inexact->exact h))))))
    
    ;; ------------------------------------------------------------------------
    (define OUT-ERROR
      "The conversion function must produce a number; result: ~e")
    
    ;; ============================================================================
    ;; MODEL
    ;; 2int : num -> int
    ;; to convert a number into an exact integer 
    (define (2int x)
      (if (number? x)
          (inexact->exact (round x))
          (error 'convert OUT-ERROR x)))
    
    ;; f2c : num -> num
    ;; to convert a Fahrenheit temperature into a Celsius temperature 
    (define (f2c f)
      (2int (* 5/9 (- f 32))))
    
    ;; fahr->cel : num -> num 
    ;; student-supplied function for converting F to C
    (define (fahr->cel f)
      (error 'convert "not initialized"))
    
    ;; slider-cb : slider% event% -> void
    ;; to use fahr->cel to perform the conversion 
    (define (slider-cb c s)
      (send sliderC set-current-x
	    ((compose in-slider-range 2int fahr->cel)
             (send sliderF get-value))))
    
    ;; in-slider-range : number -> number 
    ;; to check and to convert the new temperature into an appropriate scale 
    (define (in-slider-range x)
      (cond
        [(<= SLI-MIN x SLI-MAX) (/ (- x SLI-MIN) (- SLI-MAX SLI-MIN))]
        [else (error 'convert-gui "result out of range for Celsius slider")]))
    
    ;; to-string : number[0.,1.] -> number[SLI-MIN,SLI-MAX]
    (define (to-string x)
      (number->string (+ (* x (- SLI-MAX SLI-MIN)) SLI-MIN)))
    
    
    #| --------------------------------------------------------------------
    
    view  (exports sliderF sliderC SLI-MIN SLI-MAX) (imports f2c slider-cb)   
    
    model (imports sliderF sliderC SLI-MIN SLI-MAX) (exports f2c slider-cb)   
    
    ----------------------------------------------------------------------- |#
    
    ;; ============================================================================
    ;; VIEW
    
    (define frame (make-object frame% "Fahrenheit to Celsius Conversion"))
    (define main-panel (make-object horizontal-panel% frame))
    
    ;; create labels; aligned with sliders 
    (define mpanel (make-object vertical-panel% main-panel))
    (begin
      (make-object message% "Fahrenheit" mpanel)
      (make-object message% "" mpanel)
      (make-object message% "Celsius" mpanel))
    
    (define panel (make-object vertical-panel% main-panel))
    (send panel set-alignment 'center 'center)
    
    (define SLI-MIN (f2c -50))
    (define SLI-MAX (f2c 250))
    (define F-SLI-0 32)    
    
    ;; sliderF : slider% 
    ;; to display the Fahrenheit temperature 
    (define sliderF (make-object slider% #f -50 250 panel void F-SLI-0))
    
    ;; sliderC : slider% 
    ;; to display the Celsius temperature 
    (define sliderC (make-object scale% to-string panel))
    (define _set-sliderC (send sliderC set-current-x (in-slider-range (f2c F-SLI-0))))
    
    ;; convert : button%
    ;; to convert fahrenheit to celsius 
    (define convert (make-object button% "Convert" main-panel slider-cb))
    
    (define close (make-object button% "Close" main-panel 
                    (lambda (x e) (send frame show #f))))
    
    ;; convert-gui : (num -> num) -> void
    ;; to install f as the temperature converter 
    ;; effect: to create a window with two rulers for converting F to C
    (define (convert-gui f)
      (check-proc 'convert-gui f 1 "convert-gui" "one argument")
      (set! fahr->cel f)
      (send frame show #t))
    
    ;; ============================================================================
    ;; convert-repl : (num -> num) -> void
    ;; to start a read-eval-print loop that reads numbers [temp in F], applies f, and prints 
    ;; the result; effects: read and write; 
    ;; exit on x as input
    (define (convert-repl f)
      (check-proc 'convert-repl f 1 "convert-repl" "one argument")
      (let repl ()
        (begin
          (printf "Enter Fahrenheit temperature and press <enter> [to exit, type x]: ")
          (flush-output)
          (let* ([ans (read)])
            (cond
	      [(not (or (number? ans) (and (symbol? ans) (eq? ans 'x))))
	       (printf "The input must be a number. Given: ~s~n" ans) (repl)]
              [(eq? ans 'x) (void)]
              [(number? ans) 
               (let ([res (f ans)])
                 (if (number? res)
                     (printf "~sF corresponds to ~sC~n" ans res) 
                     (error 'convert OUT-ERROR res))
                 (repl))]
	      [else (error 'convert "can't happen")])))))
    
    ;; ============================================================================
    
    ;; make-reader-for-f : (number -> number) -> ( -> void)
    ;; make-reader-for-f creates a function that reads numbers from a file
    ;; converts them accoring to f, and prints the results
    ;; effect: if any of the S-expressions in the file aren't numbers or
    ;;         if any of f's results aren't numbers,
    ;;         the function signals an error
    (define (make-reader-for f)
      (local ((define (read-until-eof)
		(let ([in (read)])
		  (cond
		    [(eof-object? in) (void)]
		    [(number? in) (begin (check-and-print (f in)) (read-until-eof))]
		    [else (error 'convert "The input must be a number. Given: ~e~n" in)])))
	      (define (check-and-print out)
		(cond
		  [(number? out) (printf "~s~n" out)]
		  [else (error 'convert OUT-ERROR out)])))
	read-until-eof))
    
    ;; convert-file : str (num -> num) str -> void
    ;; to read a number from file in, to convert it with f, and to write it to out
    (define (convert-file in f out)
      (check-arg 'convert-file (string? in) "string" "first" in)
      (check-arg 'convert-file (file-exists? in) "name of existing file" "first" in)
      (check-proc 'convert-file f 1 "convert-file" "one argument")
      (check-arg 'convert-file (string? out) "string" "third" out)
      (when (file-exists? out)
        (delete-file out))
      (with-output-to-file out
        (lambda ()
          (with-input-from-file in (make-reader-for f)))))
    ))

(compound-unit/sig
  (import (PLT : plt:userspace^))
  (link
   (XXX : convertS (convertU ERR PLT))
   (ERR : errorS (errorU)))
  (export (open XXX)))
