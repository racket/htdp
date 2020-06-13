#lang at-exp racket/base

(provide
  grammar
  i1-2-expl
  i2-3-expl
  pre-defined-fun
  dots
  i1-2
  i2-3
  (all-from-out scribble/manual))

;; -----------------------------------------------------------------------------
(require scribble/manual)

@; -----------------------------------------------------------------------------
(define dots (bold "..."))

(define htdp "https://htdp.org/2020-5-6/Book/")
(define i1-2 (string-append htdp "i1-2.html"))
(define i2-3 (string-append htdp "i2-3.html"))

(define grammar
  @list{The grammar notation uses the notation @racket[X #, @dots] (bold
  dots) to indicate that @racket[X] may occur an arbitrary number of times
  (zero, one, or more). Separately, the grammar also defines @racket[...] as an
  identifier to be used in templates.}) 

(define i1-2-expl
  @list{See @link[i1-2]{How to Design Programs/2e, Intermezzo 1} for an
  explanation of the Beginning Student Language.})

(define i2-3-expl
  @list{See @link[i2-3]{How to Design Programs/2e, Intermezzo 2} for an explanation
  of quoted lists.})

(define pre-defined-fun
  @list{The remaining subsections list those functions that are built into the
  programming language. All other functions are imported from a teachpack or
  must be defined in the program. })
