"The library provides operations for dealing with primitive HTML documents. 

An _annotation_ is a string with 
     #\< as the first and
     #\> as the last char.

An _end annotation is a string with 
     #\< as the first, 
     #\/ as the second, and 
     #\> as the last char.

It provides: 
  annotation? 
  end-annotation
  write-file."

(reference-file "error-lib.ss")

(define-signature docS (atom? annotation? end-annotation write-file))

(define docU 
  (unit/sig docS (import errorS plt:userspace^)

    ;; mk-annotation : 
    ;;  (str number[str-lenght] -> bool) -> (any-value -> bool)
    (define (mk-annotation test)
      (lambda (val)
	(and (symbol? val) 
	  (let ((str (symbol->string val)))
	    (test str (sub1 (string-length str)))))))

    ;; annotation? : any-value -> bool
    (define annotation? 
      (mk-annotation (lambda (str E);; str : string, E : integer
		       (and (> E 1) 
			 (eqv? (string-ref str 0) #\<)
			 (eqv? (string-ref str E) #\>)))))

    ;; end-annotation? : any-value -> bool
    (define end-annotation? 
      (mk-annotation (lambda (str E);; str : string, E : integer 
		       (and (> E 2)
			 (eqv? (string-ref str 0) #\<)
			 (eqv? (string-ref str 1) #\/)
			 (eqv? (string-ref str E) #\>)))))

    ;; end-annotation : annotation -> end-annotation
    (define (end-annotation ann)
      (check-arg 'end-annotation (annotation? ann) "annotation (str)" "first" ann)
      (let ((str (symbol->string ann))) 
	(string->symbol
	  (string-append "</" (substring str 1 (string-length str))))))

    ;; line-breaking? : any-value -> bool
    (define (line-breaking? x)
      (and (annotation? x) (memq x LNBRK) #t))

    (define LNBRK 
      (let ((x '(<html> <title> <body> <table> <ol> <li> <tr>)))
	(append x (map end-annotation x))))
    
    ;; where to break lines in write-file
    (define MAX-COLUMN 80)
    
    ;; atom? : TSV -> boolean
    ;; to determine whether x is a symbol, number, or string
    (define (atom? x)
      (or (symbol? x) (number? x) (string? x)))

    ;; write-file : list-of-atom [file-name] -> void
    ;; effect: write los to file-name, in small column width
    ;;   delete file-name if it exists
    (define (write-file  los . file-name)
      ;; the-port : an output port, 
      ;;   determined from the optional second argument
      (define the-port
        (if (null? file-name)
            (current-output-port)
            (let ((the-name (car file-name)))
              (check-arg 'write-file 
                         (string? the-name)
                         "string" "(optional) second" 
                         the-name)
              (when (file-exists? the-name)
                (delete-file the-name))
              (open-output-file the-name))))
      
      (check-arg 'write-file
                 (and (list? los) (andmap atom? los)) "list of symbols" "first" los)
      ;; --- the LOOP ---
      (let loop ((i 0) (los los))
	(if (null? los) (newline the-port)
	  (let* ((wrd (first los))
		 (str (format "~a" wrd))
		 (j   (+ i (string-length str) 1)))
	    (cond
	      ((> i MAX-COLUMN)      (newline the-port) 
	       (loop 0 los))
	      ((line-breaking? wrd)  (newline the-port) 
                                     (display wrd the-port) 
                                     (newline the-port)
                                     (loop j (rest los)))

	      (else                  (display wrd the-port)
                                     (display #\SPACE the-port)
                                     (loop j (rest los)))))))
      (close-output-port the-port))

    ))

(compound-unit/sig
  (import (PLT : plt:userspace^))
  (link
    (ERR  : errorS (errorU))
    (DRAW : docS (docU ERR PLT)))
  (export (open DRAW)))
