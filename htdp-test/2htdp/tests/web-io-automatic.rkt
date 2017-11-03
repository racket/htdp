#lang racket

(require 2htdp/web-io)

;; -----------------------------------------------------------------------------
;; tests 

(require rackunit)

(check-exn exn:fail? (lambda () (show-in-browser #f)))
(check-exn exn:fail?
  (lambda ()
    (show-in-browser '(html ([a 10]) (body (b "hello world"))))))
