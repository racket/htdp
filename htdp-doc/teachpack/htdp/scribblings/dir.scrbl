#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label (except-in racket/base file-size 
	               make-date
		       date-year
		       date-month
		       date-day)
                     teachpack/htdp/dir
                     racket/contract))

@teachpack["dir"]{Working with Files and Directories}

@;declare-exporting[teachpack/htdp/dir]
@defmodule[#:require-form beginner-require htdp/dir]

The teachpack provides structures and functions for working with files and
directories: 

@defstruct[dir ([name string?][dirs (listof dir?)][files (listof file?)])]{}

@defstruct[file ([name string?][size integer?] [date (or/c 0 date?)] [content (listof char?)])]{	
represents files in the teaching languages. The struct's @racket[date] field is
optional for clients. Calling @racket[make-field] with three arguments
fills the time field with @racket[0].}

@defproc[(create-dir [path string?]) dir?]{
 Turns the directory found at @racket[path] on your computer into an instance of @racket[dir?].}

@defstruct[date ([year natural-number/c][month natural-number/c][day natural-number/c]
		 [hours natural-number/c][minutes natural-number/c][seconds natural-number/c])]{
 represents dates for file construction}


Sample: Set teachpack to @filepath{dir.rkt} and click Run:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
> (create-dir ".")
(make-dir
  "."
  '()
  (cons (make-file "arrow.scrbl" 1897 (make-date 15 1 15 11 22 21) "")
    (cons (make-file "convert.scrbl" 2071 (make-date 15 1 15 11 22 21) "")
      (cons (make-file "dir.scrbl" 1587 (make-date 8 7 8 9 23 52) "")
	(cons (make-file "docs.scrbl" 1259 (make-date 15 1 15 11 22 21) "")
	  (cons (make-file "draw.scrbl" 5220 (make-date 15 1 15 11 22 21) "")
	    (cons (make-file "elevator.scrbl" 1110 (make-date 15 1 15 11 22 21) ""))))))))
))
@;%

@(begin
#reader scribble/comment-reader
(racketblock

))
Using ``.'' usually means the directory in which your program is
located. In this case, the directory contains no sub-directories and six
files. 
 
Note: Soft links are always treated as if they were empty files.
