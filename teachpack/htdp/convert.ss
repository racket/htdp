;; TODO: docs, htdp update 
(define-signature convertS (convert-gui convert-repl convert-file))
(require-library "error.ss" "htdp")

(define convertU 
  (unit/sig convertS 
    (import errorS plt:userspace^)
    
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
    
    (define (slider-cb c s)
      (send sliderC set-value ((compose 2int fahr->cel) (send sliderF get-value))))
    
    #| --------------------------------------------------------
    
    view  (exports sliderF sliderC) (imports f2c slider-cb)   
    
    model (imports sliderF sliderC) (exports f2c slider-cb)   
    
    -------------------------------------------------------- |#
    
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
    
    ;; sliderF : slider% 
    ;; to display the Fahrenheit temperature 
    (define sliderF (make-object slider% #f -50 250 panel void 32))
    
    ;; sliderC : slider% 
    ;; to display the Celsius temperature 
    (define sliderC (make-object slider% #f (f2c -50) (f2c 250) panel void (f2c 32)))
    
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
          (printf "Enter Fahrenheint temperature and press <enter> [to exit, type x]: ")
          (flush-output)
          (let* ([ans (read)])
            (cond
              [(eq? ans 'x) (void)]
              [(number? ans) 
               (let ([res (f ans)])
                 (if (number? res)
                     (printf "~sF corresponds to ~sC~n" ans res) 
                     (error 'convert OUT-ERROR res))
                 (repl))]
              [else (printf "The input must be a number. Given: ~e~n" ans) (repl)])))))
    
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
      (check-arg 'convert-file (and (string? in) (file-exists? in)) "file name" "first" in)
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
