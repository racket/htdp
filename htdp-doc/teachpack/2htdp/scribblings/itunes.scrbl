#lang scribble/doc

@(require scribble/manual "shared.rkt" scribble/eval racket/sandbox
          (for-label lang/htdp-beginner
	             (only-in lang/htdp-beginner check-expect)
                     teachpack/2htdp/itunes))
@;(require scribble/struct )

@(define my-eval
   (let ([e (make-base-eval)])
     (e '(require 2htdp/itunes))
     e))

@(define (track) @tech[#:tag-prefixes '("itunes-data")]{Track})
@(define (date)  @tech[#:tag-prefixes '("itunes-data")]{Date})
@(define (association) @tech[#:tag-prefixes '("itunes-data")]{Association})
@(define (ltracks) @tech[#:tag-prefixes '("itunes-data")]{LTracks})
@(define (llists) @tech[#:tag-prefixes '("itunes-data")]{LLists})
@(define (lassoc) @tech[#:tag-prefixes '("itunes-data")]{LAssoc})
@(define (bsl) @tech[#:tag-prefixes '("itunes-data")]{BSDN})

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

@; ---------------------------------------------------------------------------------------------------
@section[#:tag-prefix "itunes-data"]{Data Definitions}

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

In this context, we introduce the following data definitions: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
;; @deftech{Track} is a @racket[track?]
;; @deftech{Date} is @racket[date?]

;; @deftech{LTracks} is one of:
;; -- @racket['()]
;; -- @racket[(cons #, @track[] #, @ltracks[])]
 
;; @deftech{LLists} is one of:
;; -- @racket['()]
;; -- @racket[(cons #, @lassoc[] #, @llists[])]

;; @deftech{LAssoc} is one of: 
;; -- @racket['()]
;; -- @racket[(cons #, @association[] #, @lassoc[])]

;; @deftech{Association} is @racket[(cons string? (cons #, @bsl[] '()))]

;; @deftech{BSDN} satisfies either @racket[string?], @racket[integer?], @racket[real?], @date[], or @racket[boolean?].
))
@;%

@; ---------------------------------------------------------------------------------------------------
@section[#:tag-prefix "itunes-api"]{Exported Funcions}

@defproc[(read-itunes-as-lists [file-name string?]) #, @llists[]]{
@;
creates a list-of-lists representation for all tracks in
@racket[file-name], an XML export from an iTunes library.
 
@bold{Effect} reads an XML document from @racket[file-name] 

Example:
@racketblock[
(read-itunes-as-lists "Library.xml")
]
}

@defproc[(read-itunes-as-tracks [file-name string?]) #, @ltracks[]]{
@;
creates a list-of-tracks representation for all tracks in
@racket[file-name], an XML export from an iTunes library.

@bold{Effect} reads an XML document from @racket[file-name] 

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

@interaction[#:eval my-eval
(create-track "one"
              "two"
	      "three"
	      4
	      5
	      (create-date 1 2 3 4 5 6)
	      7
	      (create-date 1 2 3 4 5 6))
(create-track "one" "two" "three" 4 5 "a date" 7 "another date")
]

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

@interaction[#:eval my-eval
(create-date 1 2 3 4 5 6)
(create-date 1 2 3 "four" 5 6)
]

}

In addition to the above, the teachpack exports the predicates for
@track[] and @date[] plus all selectors:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
 track?
 track-name
 track-artist
 track-album
 track-time
 track-track#
 track-added
 track-play#
 track-played

 date?
 date-year
 date-month
 date-day
 date-hour
 date-minute
 date-second
))
@;%
