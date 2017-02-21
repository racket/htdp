#lang racket/base
(require "private/value-turtles.rkt"
         "private/value-turtles-reader.rkt"
         racket/contract
         pict)
(provide
 (contract-out
  [turtles? (-> any/c boolean?)]
  [turtles (case->
            (-> (and/c real? positive?)
                (and/c real? positive?)
                turtles?)
            (-> (and/c real? positive?)
                (and/c real? positive?)
                real? real? real?
                turtles?))]
  [move (-> real? turtles? turtles?)]
  [draw (-> real? turtles? turtles?)]
  [turn (-> real? turtles? turtles?)]
  [turn/radians (-> real? turtles? turtles?)]
  [merge (->* (turtles?) #:rest (listof turtles?) turtles?)]
  [clean (-> turtles? turtles?)]
  [turtle-state (-> turtles? (listof (vector/c real? real? real?
                                               #:immutable #t
                                               #:flat? #t)))]
  [restore-turtle-state (-> turtles?
                            (listof (vector/c real? real? real?
                                              #:immutable #t
                                              #:flat? #t))
                            turtles?)]
  [turtles-width (-> turtles? (and/c real? positive?))]
  [turtles-height (-> turtles? (and/c real? positive?))]
  [turtles-pict (-> turtles? pict?)]))
