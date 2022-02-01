#lang at-exp scheme/base

(require "teachprims.rkt" "and-or-map.rkt"
         lang/posn
         mzlib/etc
         (only-in racket/list argmin argmax)
         syntax/docprovide
         (for-syntax scheme/base))

;; Documents the procedures:
(require "provide-and-scribble.rkt")

(provide-and-scribble
 procedures

 (begin
   (require scribble/manual scribble/examples (except-in scribble/eval examples) "sl-eval.rkt")
   (define (isl)
     (define eve
       (isl+-eval
        [(define i 3)
         (define a-list '(0 1 2 3 4 5 6 7 8 9))
         (define threshold 3)]))
     (set! isl (lambda () eve))
     eve)
 
  @(define ev-isl (isl)))
 
 (all-from-except intermediate:
                  lang/private/intermediate-funs
                  procedures
                  + * / =
                  append
                  string-append
                  string=? 
                  string<?  
                  string>?  
                  string<=?  
                  string>=?  
                  string-ci=?   
                  string-ci<?   
                  string-ci>?   
                  string-ci<=?  
                  string-ci>=?
                  andmap
                  apply
                  argmax
                  argmin
                  build-list
                  build-string
                  compose
                  filter
                  foldl
                  foldr
                  posn
                  map
                  memf
                  ormap
                  sort
                  quicksort
                  procedure?)
 
 ("Numbers (relaxed conditions plus)"
  @defproc[(= [x number] ...) number]{
 Compares numbers for equality.
 In ISL and up: @racket[=] works when applied to only one number.
 @interaction[#:eval (isl) (= 10 10) (= 11) (= 0)]
}
  @defproc[(+ [x number] ...) number]{
 Adds all given numbers.
 In ISL and up: @racket[+] works when applied to only one number or none. 
 @interaction[#:eval (isl) (+ 2/3 1/16) (+ 3 2 5 8) (+ 1) (+)]
}
  @defproc[(* [x number] ...) number]{
 Multiplies all given numbers.
 In ISL and up: @racket[*] works when applied to only one number or none. 
 @interaction[#:eval (isl) (* 5 3) (* 5 3 2) (* 2) (*)]
}
  @defproc[(/ [x number] [y number] ...) number]{
 Divides the first by all remaining numbers.
 In ISL and up: @racket[/] computes the inverse when applied to one number. 
 @interaction[#:eval (isl) (/ 12 2) (/ 12 2 3) (/ 3)]
}
  )

 ("String (relaxed conditions)"
  @defproc[(string-append [s string] ...) string]{
 Concatenates the characters of several strings. 
 @interaction[#:eval (isl) (string-append "hello" " " "world" " " "good bye")]}

  @defproc[(string=? [s string][t string][x string] ...) boolean?]{
 Determines whether all strings are equal, character for character.
 @interaction[#:eval (isl) (string=? "hello" "world") (string=? "bye" "bye")]}

  @defproc[(string<? [s string][t string][x string] ...) boolean?]{
 Determines whether the strings are ordered in a lexicographically strictly increasing manner.
 @interaction[#:eval (isl) (string<? "hello" "world" "zoo")]}

  @defproc[(string>? [s string][t string][x string] ...) boolean?]{
 Determines whether the strings are ordered in a lexicographically strictly decreasing manner.
 @interaction[#:eval (isl) (string>?  "zoo" "world" "hello")]}
    
  @defproc[(string<=? [s string][t string][x string] ...) boolean?]{
 Determines whether the strings are ordered in a lexicographically increasing manner.
 @interaction[#:eval (isl) (string<=? "hello" "hello" "world" "zoo")]}
    
  @defproc[(string>=? [s string][t string][x string] ...) boolean?]{
 Determines whether the strings are ordered in a lexicographically decreasing manner.
 @interaction[#:eval (isl) (string>=?  "zoo" "zoo" "world" "hello")]}
    
  @defproc[(string-ci=?  [s string][t string][x string] ...) boolean?]{
 Determines whether all strings are equal, character for character, regardless of case. 
 @interaction[#:eval (isl) (string-ci=?  "hello" "HellO")]}
    
  @defproc[(string-ci<?  [s string][t string][x string] ...) boolean?]{
 Determines whether the strings are ordered in a lexicographically
 strictly increasing and case-insensitive manner.
 @interaction[#:eval (isl) (string-ci<? "hello" "WORLD" "zoo")]}
    
  @defproc[(string-ci>?  [s string][t string][x string] ...) boolean?]{
 Determines whether the strings are ordered in a lexicographically
 strictly decreasing and case-insensitive manner.
 @interaction[#:eval (isl) (string-ci>?  "zoo" "WORLD" "hello")]}
    
  @defproc[(string-ci<=? [s string][t string][x string] ...) boolean?]{
 Determines whether the strings are ordered in a lexicographically
 increasing and case-insensitive manner. 
 @interaction[#:eval (isl) (string-ci<=? "hello" "WORLD" "zoo")]}
    
  @defproc[(string-ci>=? [s string][t string][x string] ...) boolean?]{
 Determines whether the strings are ordered in a lexicographically
 decreasing and case-insensitive manner.
 @interaction[#:eval (isl) (string-ci>?  "zoo" "WORLD" "hello")]}
  )
  
 ("Posn"
  @defproc[(posn) signature]{Signature for posns.})

 ("Lists"
  @defproc[((intermediate-append append) [l (listof any)] ...) (listof any)]{
 Creates a single list from several, by concatenation of the items.
 In ISL and up: @racket[append] also works when applied to one list or none. 
 @interaction[#:eval (isl)
              (append (cons 1 (cons 2 '())) (cons "a" (cons "b" '())))
              (append)]}
  )
 
 ("Higher-Order Functions (with Lambda)"
  @defproc[((intermediate-map map) [f (X ... -> Z)] [l (listof X)] ...) (listof Z)]{
 Constructs a new list by applying a function to each item on one or
 more existing lists:
 @codeblock{(map f (list x-1 ... x-n)) = (list (f x-1) ... (f x-n))}
 @codeblock{(map f (list x-1 ... x-n) (list y-1 ... y-n)) = (list (f x-1 y-1) ... (f x-n y-n))}

 @interaction[#:eval ev-isl (map add1 (list 3 -4.01 2/5))]

 @examples[#:eval ev-isl #:label #f
 (define (tag-with-a x) 
   (list "a" (+ x 1)))
 ]

 @interaction[#:eval ev-isl (map tag-with-a (list 3 -4.01 2/5))]

 @examples[#:eval ev-isl #:label #f
 (define (add-and-multiply x y) 
   (+ x (* x y)))
 ]

 @interaction[#:eval ev-isl (map add-and-multiply (list 3 -4 2/5) '(1 2 3))]
}
  @defproc[(for-each [f (any ... -> any)] [l (listof any)] ...) void?]{
 Applies a function to each item on one or more lists for effect only:
 @codeblock{(for-each f (list x-1 ... x-n)) = (begin (f x-1) ... (f x-n))}
 @interaction[#:eval (asl-eval)
              (for-each (lambda (x) (begin (display x) (newline))) '(1 2 3))
              ]
}
  @defproc[((intermediate-filter filter) [p? (X -> boolean)] [l (listof X)]) (listof X)]{
 Constructs a list from all those items on a list for which the predicate holds.
 @interaction[#:eval (isl)
              (filter odd? '(0 1 2 3 4 5 6 7 8 9))
              threshold
              (filter (lambda (x) (>= x threshold)) '(0 1 2 3 4 5 6 7 8 9))
              ]
}
  @defproc[((intermediate-foldr foldr) [f (X ... Y -> Y)] [base Y] [l (listof X)] ...) Y]{
 @codeblock{(foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))}
 @codeblock{(foldr f base (list x-1 ... x-n) (list y-1 ... y-n))
  = (f x-1 y-1 ... (f x-n y-n base))}
 @interaction[#:eval (isl)
              (foldr + 0 '(0 1 2 3 4 5 6 7 8 9))
              a-list
              (foldr (lambda (x r) (if (> x threshold) (cons (* 2 x) r) r)) '() a-list)
              (foldr (lambda (x y r) (+ x y r)) 0 '(1 2 3) '(10 11 12))
              ]
}
  @defproc[((intermediate-foldl foldl) [f (X ... Y -> Y)] [base Y] [l (listof X)] ...) Y]{
 @codeblock{(foldl f base (list x-1 ... x-n)) = (f x-n ... (f x-1 base))}
 @codeblock{(foldl f base (list x-1 ... x-n) (list x-1 ... x-n))
  = (f x-n y-n ... (f x-1 y-1 base))}
 @interaction[#:eval (isl)
              (foldl + 0 '(0 1 2 3 4 5 6 7 8 9))
              a-list
              (foldl (lambda (x r) (if (> x threshold) (cons (* 2 x) r) r)) '() a-list)
              (foldl (lambda (x y r) (+ x y r)) 0 '(1 2 3) '(10 11 12))
              ]
}
  @defproc[(build-list [n nat] [f (nat -> X)]) (listof X)]{
 Constructs a list by applying @racket[f] to the numbers between @racket[0] and @racket[(- n 1)]:
 @codeblock{(build-list n f) = (list (f 0) ... (f (- n 1)))}
 @interaction[#:eval (isl)
              (build-list 22 add1)
              i
              (build-list 3 (lambda (j) (+ j i)))
              (build-list 5
                          (lambda (i)
                            (build-list 5
                                        (lambda (j)
                                          (if (= i j) 1 0)))))
              ]
}
  @defproc[((intermediate-build-string build-string) [n nat] [f (nat -> char)]) string]{
 Constructs a string by applying @racket[f] to the numbers between @racket[0] and
 @racket[(- n 1)]: 
 @codeblock{(build-string n f) = (string (f 0) ... (f (- n 1)))}
 @interaction[#:eval (isl)
              (build-string 10 integer->char)
              (build-string 26 (lambda (x) (integer->char (+ 65 x))))]
}
  @defproc[((intermediate-quicksort quicksort) [l (listof X)] [comp (X X -> boolean)]) (listof X)]{
 Sorts the items on @racket[l], in an order according to @racket[comp] (using the quicksort
 algorithm).
 @interaction[#:eval (isl)
              (quicksort '(6 7 2 1 3 4 0 5 9 8) <)]
}
  @defproc[((intermediate-sort sort) [l (listof X)] [comp (X X -> boolean)]) (listof X)]{
 Sorts the items on @racket[l], in an order according to @racket[comp].
 @interaction[#:eval (isl)
              (sort '(6 7 2 1 3 4 0 5 9 8) <)]
}
  @defproc[((intermediate-andmap andmap) [p? (X ... -> boolean)] [l (listof X) ...]) boolean]{
 Determines whether @racket[p?] holds for all items of @racket[l] ...:
 @codeblock{(andmap p (list x-1 ... x-n)) = (and (p x-1) ... (p x-n))}
 @codeblock{(andmap p (list x-1 ... x-n) (list y-1 ... y-n)) = (and (p x-1 y-1) ... (p x-n y-n))}
 @interaction[#:eval (isl)
              (andmap odd? '(1 3 5 7 9))
              threshold 
              (andmap (lambda (x) (< x threshold)) '(0 1 2))
              (andmap even? '())
              (andmap (lambda (x f) (f x)) (list 0 1 2) (list odd? even? positive?))
              ]
}
  @defproc[((intermediate-ormap ormap)   [p? (X -> boolean)] [l (listof X)]) boolean]{
 Determines whether @racket[p?] holds for at least one items of @racket[l]:
 @codeblock{(ormap p (list x-1 ... x-n)) = (or (p x-1) ... (p x-n))}
 @codeblock{(ormap p (list x-1 ... x-n) (list y-1 ... y-n)) = (or (p x-1 y-1) ... (p x-n y-n))}
 @interaction[#:eval (isl)
              (ormap odd? '(1 3 5 7 9))
              threshold 
              (ormap (lambda (x) (< x threshold)) '(6 7 8 1 5))
              (ormap even? '())
              (ormap (lambda (x f) (f x)) (list 0 1 2) (list odd? even? positive?))
              ]
}
  @defproc[(argmin [f (X -> real)] [l (listof X)]) X]{
 Finds the (first) element of the list that minimizes the output of the function.
 @interaction[#:eval (isl)
              (argmin second '((sam 98) (carl 78) (vincent 93) (asumu 99)))
              ]
}
  @defproc[(argmax [f (X -> real)] [l (listof X)]) X]{
 Finds the (first) element of the list that maximizes the output of the function.
 @interaction[#:eval (isl)
              (argmax second '((sam 98) (carl 78) (vincent 93) (asumu 99)))
              ]
}
  @defproc[(memf [p? (X -> any)] [l (listof X)]) (union #false (listof X))]{
 Produces @racket[#false] if @racket[p?] produces @racket[false] for all
 items on @racket[l]. If @racket[p?] produces @racket[#true] for any of
 the items on @racket[l], @racket[memf] returns the sub-list starting
 from that item.
 @interaction[#:eval (isl)
              (memf odd? '(2 4 6 3 8 0))
              ]
} 
  @defproc[(apply [f (X-1 ... X-N -> Y)] [x-1 X-1] ... [l (list X-i+1 ... X-N)]) Y]{
 Applies a function using items from a list as the arguments:
 @codeblock{(apply f (list x-1 ... x-n)) = (f x-1 ... x-n)}
 @interaction[#:eval (isl)
              a-list
              (apply max a-list)
              ]
}
  @defproc[(compose [f (Y -> Z)] [g (X -> Y)]) (X -> Z)]{
 Composes a sequence of procedures into a single procedure:
 @codeblock{(compose f g) = (lambda (x) (f (g x)))}
 @interaction[#:eval (isl)
              ((compose add1 second) '(add 3))
              (map (compose add1 second) '((add 3) (sub 2) (mul 4)))
              ]
}
  @defproc[(procedure? [x any]) boolean?]{
 Produces true if the value is a procedure.
 @interaction[#:eval (isl)
              (procedure? cons)
              (procedure? add1) 
              (procedure? (lambda (x) (> x 22)))
              ]
}
  )
 )
