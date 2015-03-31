#lang racket/base

;; the posn struct for the teaching languages

(provide struct:posn make-posn posn? posn-x posn-y set-posn-x! set-posn-y!
         beginner-posn*
         (rename-out (posn posn-id))
         (rename-out (posn* posn)))

(require lang/private/signature-syntax racket/match
         (for-syntax #;"requiring from" lang/private/firstorder #;"avoids load cycle")
         ; (rename-in lang/prim (first-order->higher-order f2h))
         (for-syntax racket/base))

(define-match-expander posn*
  ;; the match expander 
  (lambda (stx)
    (syntax-case stx ()
      [(_ x y) #'(struct posn (x y))

              #;(and (? posn? the-posn)
                      (app (first-order->higher-order posn-x) x)
                      (app (first-order->higher-order posn-y) y))]))
  ;; the run-time values
  (lambda (stx)
    (syntax-case stx ()
      ;; a signature 
      [x (identifier? #'x) #'posn-signature]
      ;; everything else remains a syntax error 
      [_
       (let ([stx* (cons #'posn-signature (cdr (syntax-e stx)))])
         (datum->syntax stx stx*))])))

(define-match-expander beginner-posn*
  ;; the match expander 
  (lambda (stx)
    (syntax-case stx ()
      [(_ x y) #'(struct posn (x y))]))
  ;; the run-time values
  (lambda (stx)
    (syntax-case stx ()
      ;; a signature 
      [x (identifier? #'x) (raise-syntax-error #f "this variable is not defined" stx)]
      ;; everything else remains a syntax error 
      [_ (raise-syntax-error #f "this function is not defined" stx)])))

(struct posn (x y) #:mutable #:transparent)

;; We define a separate function so tha it has the 
;; name `make-posn':
(define (make-posn x y) (posn x y))

(define posn-signature (signature (predicate posn?)))
