
;; Implements the Beginner Scheme language, at least in terms of the
;; forms and procedures. The reader-level aspects of the language
;; (e.g., case-sensitivity) are not implemented here.

(module beginner mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "math.ss")
	   (lib "docprovide.ss" "syntax"))

  ;; Implements the forms:
  (require "private/teach.ss"
	   "private/teachprims.ss")

  ;; syntax:
  (provide (rename beginner-define define)
	   (rename beginner-define-struct define-struct)
	   (rename beginner-lambda lambda)
	   (rename beginner-app #%app)
	   (rename beginner-cond cond)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
	   (rename beginner-quote quote)
	   (rename #%plain-module-begin #%module-begin)
	   #%datum
	   #%top
	   empty true false)

  ;; procedures:
  (provide-and-document
   procedures

   ("Numbers: Integers, Rationals, Reals, Complex, Exacts, Inexacts"
    (number? (any -> boolean)
	     "to determine whether some value is a number")
    (= (num num num ... -> boolean)
       "to compare two numbers for equality")
    (< (real real real ... -> boolean)
       "to compare two real numbers for less-than")
    (> (real real real ... -> boolean)
       "to compare two real numbers for greater-than")
    (<= (real real real ... -> boolean)
	"to compare two real numbers for less-than or equality")
    (>= (real real -> boolean)
	"to compare two real numbers for greater-than or equality")
    
    ((beginner-+ +) (num num num ... -> num)
       "to compute the sum of the input numbers")
    (- (num num ... -> num)
       "to compute the difference between the input numbers or to negate a number, if there is only one input")
    ((beginner-* *) (num num num ... -> num)
       "to compute the product of all of the input numbers")
    ((beginner-/ /) (num num num ... -> num)
       "to compute the rational quotient of its input."
       "None but the first number can be zero.")
    (max (num num ... -> num)
	 "to determine the largest number")
    (min (num num ... -> num)
	 "to determine the smallest number")       
    (quotient (int int -> int)
	      "to compute the quotient of two integers")
    (remainder (int int -> int)
	       "to compute the remainder of two integers")
    (modulo (int int -> int)
	    "to compute the modulo of two integers")    
    (square (num -> num)
	    "to compute the square of a number")
    (sqrt (num -> num)
	  "to compute the square root of a number")    
    (expt (num num -> num)
	  "to compute the exponent of two numbers")
    (abs (real -> real)
	 "to compute the absolute value of a real number")
    (sgn (real -> (union 1 |#i1.0| 0 |#i0.0| -1 |#i-1.0|))
	 "to compute the sign of a real number")
    
    ;; fancy numeric 
    (exp (num -> num)
	 "to compute e raised to a number")
    (log (num -> num)
	 "to compute the base-e logarithm of a number")
    
    ;; trigonometry
    (sin (num -> num)
	 "to compute the sine of a number (radians)")
    (cos (num -> num)
	 "to compute the cosine of a number (radians)")
    (tan (num -> num)
	 "to compute the tangent of a number (radians)")
    (asin (num -> num)
	  "to compute the arcsine (inverse of sin) of a number")
    (acos (num -> num)
	  "to compute the arccosine (inverse of cos) of a number")
    (atan (num -> num)
	  "to compute the arctan (inverse of tan) of a number")    
    
    (sinh (num -> num)
	  "to compute the hyperbolic cosine of a number")
    (cosh (num -> num)
	  "to compute the hyperbolic cosine of a number")
    
    (exact? (num -> bool)
	    "to determine whether some number is exact")
    
    (integer? (any -> bool)
	      "to determine whether some value is an integer (exact or inexact)")
    
    (zero? (number -> bool)
	   "to determine if some value is zero or not")
    (odd? (integer -> bool)
	  "to determine if some value is odd or not")
    (even? (integer -> bool)
	   "to determine if some value is even or not")

    (add1 (number -> number)
	  "to compute a number one larger than a given number")
    (sub1 (number -> number)
	  "to compute a number one smaller than a given number")

    (lcm (int int ... -> int)
	 "to compute the least common multiple of two integers")
    
    (gcd (int int ... -> int)
	 "to compute the greatest common divisior")
    
    (rational? (any -> bool)
	       "to determine whether some value is rational number")
    
    (numerator (rat -> int)
	       "to compute the numerator of a rational")
    
    (denominator (rat -> int)
		 "to compute the denominator of a rational")
    
    (inexact? (num -> bool)
	      "to determine whether some number is inexact")
    
    (real? (any -> bool)
	   "to determine whether some value is a real number")
    
    (floor (real -> int)
	   "to determine the closest integer below a real number")
    
    (ceiling (real -> int)
	     "to determine the closest integer above a real number")
    
    (round (real -> int)
	   "to round a real number to an integer")
    
    (complex? (any -> bool)
	      "to determine whether some value is complex")
    
    (make-polar (real real -> num)
		"to create a complex from a magnitude and angle")
    
    (real-part (num -> real)
	       "to extract the real part from a complex number")
    
    (imag-part (num -> real)
	       "to extract the imaginary part from a complex")
    
    (magnitude (num -> real)
	       "to determine the magnitude of a complex number")
    
    (angle (num -> real)
	   "to extract the angle from a complex number")
    
    (conjugate (num -> num)
	       "to compute the conjugate of a complex number")
    
    (exact->inexact (num -> num)
		    "to convert an exact number to an inexact one")
    
    (inexact->exact (num -> num)
		    "to approximate an inexact number by an exact one")
    
					;    "Odds and ends"
    
    (number->string (num -> string)
		    "to convert a number to a string")
    
    (integer->char (int -> char)
		   "to lookup the character that corresponds to the given integer in the ASCII table (if any)")
    
    (random (int -> int)
	    "to generate a random natural number less than some given integer")
    
    (current-seconds (-> int)
		     "to compute the current time in seconds elapsed"
		     "(since a platform-specific starting date)")
    
    (e real
       "Euler's number")
    (pi real
	"the ratio of a circle's circumference to its diameter"))
   
   ("Booleans" 
    (boolean? (any -> boolean)
	      "to determine whether some value is a boolean")
    
    (boolean=? (boolean boolean -> boolean)
	       "to determine whether two booleans are equal")
    
    (not (boolean -> boolean)
	 "to compute the negation of a boolean value"))
   
   ("Symbols"
    (symbol? (any -> boolean)
	     "to determine whether some value is a symbol")
    
    (symbol=? (symbol symbol -> boolean)
	      "to determine whether two symbols are equal"))

   ("Lists"
    (cons? (any -> boolean)
	   "to determine whether some value is a constructed list")
    (pair? (any -> boolean)
	   "to determine whether some value is a constructed list")	
    (empty? (any -> boolean)
	    "to determine whether some value is the empty list")
    (null? (any -> boolean)
	   "to determine whether some value is the empty list")
    (list? (any -> boolean)
	   "to determine whether some value is a list")
    
    ((beginner-cons cons) (X (listof X) -> (listof X))
	  "to construct a list")

    (null ( empty )
	   "the empty list")

    (first ( (cons Y (listof X)) -> Y )
	   "to select the first item of a non-empty list")
    (car ( (cons Y (listof X)) -> Y )
	 "to select the first item of a non-empty list")       
    (rest ((cons Y (listof X)) -> (listof X))
	  "to select the rest of a non-empty list")
    (cdr ((cons Y (listof X)) -> (listof X))
	 "to select the rest of a non-empty list")       
    
    (second ( (cons Z (cons Y (listof X))) -> Y )
	    "to select the second item of a non-empty list")
    (cadr ( (cons Z (cons Y (listof X))) -> Y )
	  "to select the second item of a non-empty list")	  
    (cdar ( (cons (cons Z (listof Y)) (listof X)) -> (listof Y) )
	  "to select the rest of a non-empty list in a list") 
    (caar ( (cons (cons Z (listof Y)) (listof X)) -> Z )
	  "to select the first item of the first list in a list")
    (cddr ( (cons Z (cons Y (listof X))) -> (listof X) )
	  "to select the rest of the rest of a list")
    (third ( (cons W (cons Z (cons Y (listof X)))) -> Y )
	   "to select the third item of a non-empty list")
    (caddr ( (cons W (cons Z (cons Y (listof X)))) -> Y )
	   "to select the third item of a non-empty list")	  
    (caadr ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> (listof Z) )
	   "to select the rest of the first list in the first list of a list")
    (caaar ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> W )
	   "to select the first item of the first list in the first list of a list")
    (cdaar ( (cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> (listof Z) )
	   "to select the rest of the first list in the first list of a list")
    (cdadr ( (cons W (cons (cons Z (listof Y)) (listof X))) -> (listof Y) )
	   "to select the rest of the first list in the rest of a list")
    (cadar ( (cons (cons W (cons Z (listof Y))) (listof X)) -> Z )
	   "to select the second item of the first list of a list")
    (cddar ( (cons (cons W (cons Z (listof Y))) (listof X)) -> (listof Y) )
	   "to select the rest of the rest of the first list of a list")
    (cdddr ( (cons W (cons Z (cons Y (listof X)))) -> (listof X) )
	   "to select the rest of the rest of the rest of a list")
    (fourth ( (listof Y)  -> Y ) ; domain: (cons V (cons W (cons Z (cons Y (listof X)))))
	    "to select the fourth item of a non-empty list")
    (cadddr ( (listof Y)  -> Y ) ;  domain: (cons V (cons W (cons Z (cons Y (listof X)))))
	    "to select the fourth item of a non-empty list")
    (fifth ( (listof Y) -> Y ) ; domain: (cons U (cons V (cons W (cons Z (cons Y (listof X))))))
	   "to select the fifth item of a non-empty list")
    (sixth ( (listof Y) -> Y ) ;  domain: (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X)))))))
	   "to select the sixth item of a non-empty list")
    (seventh ( (listof Y) -> Y ) ;  domain: (cons S (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X))))))))
	     "to select the seventh item of a non-empty list")
    (eighth ( (listof Y) -> Y ) ;  domain: (cons R (cons S (cons T (cons U (cons V (cons W (cons Z (cons Y (listof X)))))))))
	    "to select the eighth item of a non-empty list")    
    
    (list-ref ((listof X) number -> X )
	      "to extract the i-th item from a list")
    
    (list (any ... -> (listof any)) "to construct a list of its arguments")

    (list (any ... (listof any) -> (listof any)) 
	  "to construct a list of its arguments, building on the last argument")

    ((beginner-list* list*) (any ... (listof any) -> (listof any)) 
			    "to construct a list by adding multiple items to a list")

    ((beginner-append append) ((listof any) ... -> (listof any))
	    "to create a single list from several, by juxtaposition of the items")
    (length (list -> number)
	    "to compute the number of items on a list")
    (memq (any list -> (union false list))
	  "to determine whether some value is on some list"
	  "(comparing values with eq?)")
    (memv (any list -> (union false list))
	  "to determine whether some value is on the list"
	  "(comparing values with eqv?)")
    (member (any list -> (union false list))
	    "to determine whether some value is on the list"
	    "(comparing values with equal?)")
    (reverse (list -> list)
	     "to create a reversed version of a list")
    (assq (X (listof (cons X Y)) -> (union false (cons X Y)))
	  "to determine whether some item is the first item of a pair"
	  "in a list of pairs")
    (equal? (list list -> boolean)
	    "to determine whether two lists are equal"))

   ("Posns"
    (make-posn (number number -> posn) "to construct a posn")
    (posn? (anything -> boolean) "to determine if its input is a posn")
    (posn-x (posn -> number) "to extract the x component of a posn")
    (posn-y (posn -> number) "to extract the y component of a posn"))

   ("Characters"
    (char? (any -> boolean)
	   " ")
    (char=? (char char ... -> boolean)
	    "to determine whether two characters are equal")
    (char<? (char char ... -> boolean)
	    "to determine whether a character precedes another")
    (char>? (char char ... -> boolean)
	    "to determine whether a character succeeds another")
    (char<=? (char char ... -> boolean)
	     "to determine whether a character precedes another"
	     "(or is equal to it)")
    (char>=? (char char ... -> boolean)
	     "to determine whether a character succeeds another"
	     "(or is equal to it)")
    
    (char-ci=? (char char ... -> boolean)
	       "to determine whether two characters are equal"
	       "in a case-insensitive manner")
    (char-ci<? (char char ... -> boolean)
	       "to determine whether a character precedes another"
	       "in a case-insensitive manner")
    (char-ci>? (char char ... -> boolean)
	       "to determine whether a character succeeds another"
	       "in a case-insensitive manner")
    (char-ci<=? (char char ... -> boolean)
		"to determine whether a character precedes another"
		"(or is equal to it) in a case-insensitive manner")
    (char-ci>=? (char char ... -> boolean)
		"to determine whether a character succeeds another"
		"(or is equal to it) in a case-insensitive manner")
    
    (char-numeric? (char -> boolean)
		   "to determine whether a character represents a digit")
    (char-alphabetic? (char -> boolean)
		      "to determine whether a character represents"
		      " an alphabetic character")
    (char-whitespace? (char -> boolean)
		      "to determine whether a character represents space")
    (char-upper-case? (char -> boolean)
		      "to determine whether a character is an"
		      "upper-case character")
    (char-lower-case? (char -> boolean)
		      "to determine whether a character is a"
		      "lower-case character")
    (char-upcase (char -> char)
		 "to determine the equivalent upper-case character")
    (char-downcase (char -> char)
		   "to determine the equivalent lower-case character")
    (char->integer (char -> integer)
		   "to lookup the number that corresponds to the"
		   "given character in the ASCII table (if any)"))
   ("Strings"
    (string? (any -> boolean)
	     "to determine whether a value is a string")
    (string-length (string -> nat)
		   "to determine the length of a string")
    (make-string (nat char -> string)
		 "to produce a string of given length"
		 "from a single given character")
    (string-ref (string nat -> char)
		"to extract the i-the character from a string")
    
    (substring (string nat nat -> string)
	       "to extract the substring starting at a 0-based index,"
	       " consisting of a given number of characters")
    (string-copy (string -> string)
		 "to copy a string")
    (string-append (string ... -> string)
		   "to juxtapose the characters of several strings")
    
    (string=? (string string ... -> boolean)
	      "to compare two strings character-wise")
    (string<? (string string ... -> boolean)
	      "to determine whether one string alphabetically"
	      "precedes another")
    (string>? (string string ... -> boolean)
	      "to determine whether one string alphabetically"
	      "succeeds another")
    (string<=? (string string ... -> boolean)
	       "to determine whether one string alphabetically"
	       "precedes another (or is equal to it)")
    (string>=? (string string ... -> boolean)
	       "to determine whether one string alphabetically"
	       "succeeds another (or is equal to it)")
    
    (string-ci=? (string string ... -> boolean)
		 "to compare two strings character-wise"
		 "in a case-insensitive manner")
    (string-ci<? (string string ... -> boolean)
		 "to determine whether one string alphabetically"
		 "precedes another in a case-insensitive manner")
    (string-ci>? (string string ... -> boolean)
		 "to determine whether one string alphabetically"
		 "succeeds another in a case-insensitive manner")
    (string-ci<=? (string string ... -> boolean)
		  "to determine whether one string alphabetically"
		  "precedes another (or is equal to it)"
		  "in a case-insensitive manner")
    (string-ci>=? (string string ... -> boolean)
		  "to determine whether one string alphabetically"
		  "succeeds another (or is equal to it)"
		  "in a case-insensitive manner")
    
    (string->number (string -> (union number false))
		    "to convert a string into a number,"
		    "produce false if impossible")
    (string->list (string -> (listof char))
		  "to convert a string into a list of characters")
    (list->string ((listof char) -> string)
		  "to convert a s list of characters into a string")

    (format (string any ... -> string)
	    "to format a string, possibly embedding values"))

   ("Misc"
    ((beginner-error error) (symbol string -> void) "to signal an error")
    (struct? (any -> boolean) "to determine whether some value is a structure")
    (eq? (any any -> boolean) "to compare two values based on when they were created")
    (eof (eof) "the end-of-file value")
    (eof-object? (any ->boolean) "to determine whether some value is the end-of-file value")
    ((beginner-exit exit) ( -> void) "to exit the running program"))))
