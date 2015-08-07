#lang scribble/doc

@(require scribble/manual "shared.rkt" scribble/eval
          (for-label (only-in lang/htdp-beginner check-expect)
                     teachpack/2htdp/itunes))
@(require scribble/struct)

@(define WorldState @tech[#:tag-prefixes '("world")]{WorldState})
@(define S-expression @tech[#:tag-prefixes '("universe")]{S-expression})

@; -----------------------------------------------------------------------------

@teachpack["itunes"]{iTunes}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/itunes #:use-sources (teachpack/2htdp/itunes)]

@;{FIXME: the following paragraph uses `defterm' instead of `deftech',
   because the words "world" and "universe" are used as datatypes, and
   datatypes are currently linked as technical terms --- which is a hack.
   Fix the paragraph when we have a better way to link datatype names.}

The @tt{itunes.rkt} teachpack implements and provides the functionality
 for reading the collection of tracks exported from iTunes. 

In iTunes, select @tt{Library} from the @tt{File} menu and then choose
 @tt{Export Library}. Doing so exports a description of your iTunes
 collection as a file in XML format. 

@section{Data Definitions}

@defstruct[track ([name string?]
		  [artist string?]
		  [album string?]
		  [time natural-number/c]
		  [track# natural-number/c]
		  [added date?]
		  [play# natural-number/c]
		  [played date?])]{is 
 one representations for the music tracks in an iTunes collection.
 
 An instance records that the track has title @racket[name], is produced by
 @racket[artist], belongs to @racket[album], plays for @racket[time]
 milliseconds, is positioned at @racket[track#], was added at date
 @racket[added], has been played @racket[play#] times, and was last played
 at @racket[played] date.}

@defstruct[date  ([year natural-number/c]
		  [month natural-number/c]
		  [day natural-number/c]
		  [hour natural-number/c]
		  [minute natural-number/c]
		  [second natural-number/c])]{is 
 a representations of dates in an iTunes collection.

 An instance records six pieces of information: the date's @racket[year],
 @racket[month] (between @racket[1] and @racket[12] inclusive),
 @racket[day] (between @racket[1] and @racket[31]), @racket[hour] (between
 @racket[0] and @racket[23]), @racket[minute] (between @racket[0] and
 @racket[59]), and @racket[second] (between @racket[0] and @racket[59]).}

In this context, we need the following data definitions: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
 ;; @deftech{LTracks} is one of:
 ;; -- @racket['()]
 ;; -- @racket[(cons #, @tech{Track} #, @tech{LTracks})]
 
 ;; @deftech{LLists} is one of:
 ;; -- @racket['()]
 ;; -- @racket[(cons #, @tech{Association} #, @tech{LLists})]

 ;; @deftech{Association} is @racket[(cons string? (cons #, @tech{BSL-Value} '()))]

 ;; @deftech{BSL-Value} satisfies either @racket[string?], @racket[integer?], @racket[real?], @racket[date?], or @racket[boolean?].
))
@;%

@;%
@(begin
#reader scribble/comment-reader
(racketblock
 ;; Any Any Any Any Any Any Any Any -> Track or #false
 create-track 
 track?
 track-name
 track-artist
 track-album
 track-time
 track-track#
 track-added
 track-play#
 track-played
 
 ;; Any Any Any Any Any Any -> Date or #false
 create-date
 date?
 date-year
 date-month
 date-day
 date-hour
 date-minute
 date-second)
))
@;%

@; ---------------------------------------------------------------------------------------------------

@defproc[(read-itunes-as-list [file-name string?]) @tech{LLists}]{creates a
@;
list of lists representation for all tracks in file, an XML export from an
iTunes library 
 
@bold{effect} reads an XML document from @racket[file-name] 

Example:
@racketblock[
(read-itunes-as-list "Library.xml")
]
}

@defproc[(read-itunes-as-tracks [file-name string?]) @tech{LTracks}]{
@;
creates list of tracks representation for all tracks in
file, an XML export from an iTunes library

@bold{effect} reads an XML document from @racket[file-name] 

Example:
@racketblock[
(read-itunes-as-tracks "Library.xml")
]

}

@defproc[(create-track 
		       [name string?]
		       [artist string?]
		       [album string?]
		       [time natural-number/c]
		       [track# natural-number/c]
		       [added date?]
		       [play# natural-number/c]
		       [played date?]) 
         (or/c track? false?)]{
@;
 creates a track representation if the inputs live up to their
 predicates. Otherwise it produces @racket[#false].

@bold{Note} This is a @emph{checked} constructor. 
}

@defproc[(create-date
		       [year natural-number/c]
		       [month natural-number/c]
		       [day natural-number/c]
		       [hour natural-number/c]
		       [minute natural-number/c]
		       [second natural-number/c])
         (or/c date? false?)]{
@;
 creates a date representation if the inputs live up to their
 predicates. Otherwise it produces @racket[#false].

@bold{Note} This is a @emph{checked} constructor. 
}
