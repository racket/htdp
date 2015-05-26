#lang racket/base
(require "private/value-turtles.rkt"
         racket/contract)
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
  [clean (-> turtles? turtles?)]))
