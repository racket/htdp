#lang racket

(require 2htdp/web-io)

;; -----------------------------------------------------------------------------
;; tests 

(require rackunit)

(show-in-browser
  '(html ()
     (body ([bgcolor "red"])
       (b ()
	  "hello world"
	  " "
	  "does this"))))
