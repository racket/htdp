#lang racket/base

;; the posn struct for the teaching languages

(provide struct:posn make-posn posn? posn-x posn-y set-posn-x! set-posn-y!
         Posn PosnOf
         beginner-posn*
	 (for-syntax EXPECTED-FUNCTION-NAME)
         (rename-out (posn posn-id))
         (rename-out (posn* posn)))

(require lang/private/signature-syntax racket/match
         (for-syntax #;"requiring from" lang/private/firstorder #;"avoids load cycle")
         ; (rename-in lang/prim (first-order->higher-order f2h))
         (for-syntax racket/base)
         deinprogramm/signature/signature-english
         (only-in deinprogramm/signature/signature set-signature-arbitrary! signature-arbitrary)
         (only-in deinprogramm/quickcheck/quickcheck arbitrary-record))


(define-for-syntax EXPECTED-FUNCTION-NAME
  "expected a function after the open parenthesis, but found a structure type (do you mean ~a)")

(define-match-expander posn*
  ;; the match expander 
  (lambda (stx)
    (syntax-case stx ()
      [(_ x y) #'(struct posn (x y))]))
  ;; the run-time values
  (lambda (stx)
    (syntax-case stx ()
      ;; a signature 
      [x (identifier? #'x) #'Posn]
      ;; everything else remains a syntax error 
      [(f . x)
	(raise-syntax-error #f (format EXPECTED-FUNCTION-NAME "make-posn") #'f)])))

(define-match-expander beginner-posn*
  ;; the match expander 
  (lambda (stx)
    (syntax-case stx ()
      [(_ x y) #'(struct posn (x y))]))
  ;; the run-time values
  (lambda (stx)
    (syntax-case stx ()
      [x (identifier? #'x) (raise-syntax-error #f "this variable is not defined" stx)]
      ;; everything else remains a syntax error 
      [(f . x)
       (raise-syntax-error #f  (format EXPECTED-FUNCTION-NAME "make-posn") #'f)])))

(struct posn (x y)
  #:mutable
  #:transparent
;; do not use, does not work in this context 
;;  #:extra-constructor-name make-posn 
  #:reflection-name 'posn
  )

;; We define a separate function so that it has the 
;; name `make-posn':
(define (make-posn x y) (posn x y))

(define Posn (signature posn (predicate posn?)))
(define (PosnOf x-sig y-sig)
  (let ((sig
         (make-combined-signature
          'Pos
          (list (signature (at 'Posn (predicate posn?)))
                (make-property-signature 'X
                                         posn-x
                                         x-sig
                                         'x)
                (make-property-signature 'Y
                                         posn-y
                                         y-sig
                                         'y))
          'parametric-signature)))
    (set-signature-arbitrary! sig
                              (arbitrary-record make-posn
                                                (list posn-x posn-y)
                                                (signature-arbitrary x-sig) (signature-arbitrary y-sig)))
    sig))
     
        
; FIXME: tests missing
         
