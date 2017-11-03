#lang racket

;; show an X-expression in the browser 

;; -----------------------------------------------------------------------------
;; services 

(provide
 ;; X-expression -> String
 ;; converts the given X-expression to a String
 ;; EFFECT sends the String to an external browser 
 show-in-browser)

;; -----------------------------------------------------------------------------
;; dependencies 

(require (only-in net/sendurl send-url/contents))
(require (only-in xml/xml xexpr? xexpr->string))
(require htdp/error)

;; -----------------------------------------------------------------------------
;; implementation 

(define (show-in-browser x)
  (check-the-argument x (xexpr? x))
  (define x-as-string (xexpr->string x))
  (send-url/contents x-as-string #true)
  x-as-string)

;; Any Boolean [String] -> Void 
(define (check-the-argument x y [expected "X-expression"])
  (check-arg 'show-in-browser y expected "first" x))