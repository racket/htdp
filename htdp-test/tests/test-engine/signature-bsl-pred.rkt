;; Should be free of syntax errors: `predicate` can refer to `custom-pred?` in BSL
#lang htdp/bsl

(define (custom-pred? str)
  (string-contains? "hello" str))

(: greet (String -> (predicate custom-pred?)))
(check-expect (greet "Alice") "hello Alice")
(define (greet who)
  (string-append "hello" " " who))
