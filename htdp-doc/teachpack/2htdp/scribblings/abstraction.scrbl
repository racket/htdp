#lang scribble/doc

@(require "shared.rkt" scribble/manual scribble/eval
          (for-label
	    teachpack/2htdp/abstraction
	    (only-in lang/htdp-beginner check-expect)
	    #;
	    (except-in racket for/list for/or for/and for/sum for/product for*/list for*/or for*/and for*/sum for*/product)
	    ))	

@; ---------------------------------------------------------------------------------------------------

@teachpack["universe"]{Worlds and the Universe}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/abstraction #:use-sources (teachpack/2htdp/abstraction)]

@;-----------------------------------------------------------------------------
@section[#:tag "abstraction" #:tag-prefix "x"]{Abstraction}

@defform/subs[#:id for/list
              (for/list (comprehension-clause comprehension-clause ...) body-expression)
              ([comprehension-clause (identifier expression)])]{
  loops ...

@interaction[
(for/list ((i 10))
  (list i (* i i)))
]
}
