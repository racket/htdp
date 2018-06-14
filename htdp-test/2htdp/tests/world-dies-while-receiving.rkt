#lang racket

;; When a world dies while receiving a message, universe's cleanup must work properly.
;; This lest sends a large byte string to the client and the client is going to close
;; down while receiving this message. 

(require 2htdp/universe 2htdp/image)

(define long (make-bytes 10000000 11))

(define (uni)
  (universe '()
    [on-new (lambda (worlds w) (make-bundle (cons w worlds) (list (make-mail w long)) '()))]
    [on-msg (lambda (worlds from msg) (make-bundle worlds '() '()))]))

(define (cli)
  (big-bang 10
    [register   LOCALHOST]
    [on-tick    sub1 .1]
    [stop-when  zero?]
    [on-receive (lambda (n msg) (displayln n (current-error-port)) n)]
    [to-draw    (lambda (n) (text (number->string n) 44 'red))]))

(launch-many-worlds (cli) (uni))

