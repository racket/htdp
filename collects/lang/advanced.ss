
(module advanced mzscheme
  (require "private/teach.ss"
	   "private/teachprims.ss"
	   (lib "etc.ss")
	   (lib "list.ss")
	   (lib "docprovide.ss" "syntax"))

  ;; syntax:
  (provide (rename advanced-define define)
	   (rename advanced-define-struct define-struct)
	   (rename advanced-lambda lambda)
	   (rename advanced-app #%app)
	   (rename intermediate-local local)
	   (rename advanced-let let)
	   (rename advanced-recur recur)
	   (rename intermediate-letrec letrec)
	   (rename beginner-cond cond)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
	   (rename quote quote)
	   (rename intermediate-time time)
	   (rename advanced-begin begin)
	   (rename set! set!)
	   (rename delay delay)
	   (rename #%plain-module-begin #%module-begin)
	   #%datum
	   #%top
	   empty true false)

  ;; procedures:
  (provide-and-document
   procedures

   (all-from beginner: (lib "beginner.ss" "lang") procedures)

   ("Reading and Printing"
    (printf (string any ... -> void)
	    "to format the rest of the arguments according to the first argument and print it to stdout")
    (display (any -> void)
	     "to print the argument to stdout")
    (write (any -> void)
	   "to print the argument to stdout"))
   
   ("Lists"
    (set-first! ((cons Y (listof X)) Y -> void)
		"to update the first item of a non-empty list")
    (set-rest! ((cons Y (listof X)) (listof X) -> void)
	       "to update the rest of a non-empty list")
    (set-car! ((cons Y (listof X)) Y -> void)
	      "to update the first item of a non-empty list")
    ((advanced-set-cdr! set-cdr!) ((cons Y (listof X)) (listof X) -> void)
	      "to update the rest of a non-empty list"))
   
   ("Misc"
    (force (delay -> any) "to find the delayed value. See also delay.")
    (promise? (any -> boolean) "to determine if a value is delayed."))

   ("Posns"
    (set-posn-x! (posn number -> void) "to update the x component of a posn")
    (set-posn-y! (posn number -> void) "to update the x component of a posn"))

   ("Vectors"
    (vector (X ... -> (vector X ...))
	    "to construct a vector")
    (make-vector (number X -> (vectorof X))
		 "to construct a vector")
    (build-vector (nat (nat -> X)  -> (vectorof X))
		  "to construct a vector")	
    (vector-ref ((vector X) nat -> X)
		"to extract an element from a vector")
    (vector-length ((vector X) -> nat)
		   "to determine the length of a vector")	
    (vector-set! ((vectorof X) nat X -> (vectorof X))
		 "to update a vector")
    (vector? (any -> boolean)
	     "to determine if a value is a vector"))

   ("Higher-Order Functions"
    (map ((X ... -> Z) (listof X) ... -> (listof Z))
	 "to construct a new list by applying a function to each item on an existing existing") 
    (for-each ((any ... -> any) (listof any) ... -> void)
	      "to apply a function to each item on a list for effect only")
    (filter ((X -> boolean) (listof X) -> (listof X))
	    "to construct a list from all those items on  a list for which the predicate holds")
    (foldr ((X Y -> Z) Y (listof X) -> Z)
	   "(foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))")
    (foldl ((X Y -> Z) Y (listof X) -> Z)
	   "(foldl f base (list x-1 ... x-n)) = (f x-n ... (f x-1 base))")
    (build-list (nat (nat -> X) -> (listof X))
		"(build-list n f) = (list (f 0) ... (f (- n 1)))")
    (quicksort ((X X -> boolean) (listof X) -> (listof X))
	       "to construct a list from all items on a list in an order according to a predicate")
    (andmap ((X -> boolean) (listof X) -> boolean)
	    "(andmap p (list x-1 ... x-n)) = (and (p x-1) (and ... (p x-n)))")
    (ormap ((X -> boolean) (listof X) -> boolean)
	   "(ormap p (list x-1 ... x-n)) = (or (p x-1) (and ... (p x-n)))"))

   ("Continuations"
    (call/cc ((cont -> any) -> any)
	     "to capture the current continuation")
    (call-with-current-continuation
     ((cont -> any) -> any)
     "to capture the current continuation"))

   ("Boxes"
    (box (any -> box)
	 "to construct a box")
    (unbox (box -> any)
	   "to extract the boxed value")
    (set-box! (box any -> void)
	      "to update a box")
    (box? (any -> boolean)
	  "to determine if a value is a box"))
#|
   ("Turtles"
    (turtles (-> void)
	     "to toggle the view of the turtles window")
    (turtles (bool -> void)
	     "to open or close the turtles window, based on the argument")
    (turn (number -> void)
	  "to turn the turtles by a number of degrees")
    (turn/radians (number -> void)
		  "to turn the turtles by a number of radians")
    (move (number -> void)
	  "to move the turtles forward")
    (erase (number -> void)
	   "to erase the path in front of each turtle")
    (move-offfset (number number -> void)
		  "to move by a delta-x and delta-y")
    (draw-offfset (number number -> void)
		  "to draw by a delta-x and delta-y")
    (erase-offfset (number number -> void)
		   "to erase by a delta-x and delta-y")
    (clear (-> void)
	   "to erase the drawing of the turtles entirely")
    (save-turtle-bitmap (string (union 'xbm 'xpm 'pict) -> void)
			"to save the current turtles to a platform-specific filetype")
    (splitfn ((-> void) -> void)
	     "to split the turtles (used in the expansion of `split')")
    (split*fn ((listof (-> void)) -> void)
	      "to split the turtles (used in the expansion of `split*')")
    (tpromptfn ((-> void) -> void)
	       "to set a turtle prompt (used in the expansion of `tprompt')"))
|#
))
