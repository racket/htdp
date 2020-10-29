#lang racket/base
(require (for-syntax racket/base)
         rackunit)

(define-syntax (run stx)
  (syntax-case stx ()
    [(build name exp ...)
     (let ([modname (string->symbol (format "mod~a" (syntax-line stx)))])
       (datum->syntax
        stx
        `(begin
           (module ,modname racket/base
             (require test-engine/racket-tests)
             (provide get-output)
             ,@(syntax->list #'(exp ...))
             (define sp (open-output-string))
             (parameterize ([current-output-port sp])
               (test))
             (define (get-output) (get-output-string sp)))
           (require (submod "." ,modname))
           (define ,#'name (get-output)))))]))

(run ch1 (check-expect "hello" "world"))
(check-regexp-match #rx"Actual value.*\"hello\"" ch1)
(check-regexp-match #rx"(\"world\".*expected value)|(Expected value.\"world\")" ch1)

