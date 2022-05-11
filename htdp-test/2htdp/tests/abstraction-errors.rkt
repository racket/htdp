#lang racket/load

(require rackunit)

(define-syntax-rule
  (exn/msg m e)
  (check-exn 
   (lambda (x)
     (and (exn:fail:syntax? x)
          (regexp-match m (exn-message x))))
   (lambda ()
     (eval `(module a racket/base (require ,ABSTRACTION) e)))))

(define ABSTRACTION '2htdp/abstraction)

(exn/msg "bad syntax" (for/list 10))
(exn/msg "expected at least one comprehension clause" (for/list () 10))
(exn/msg "expected comprehension clause" (for/list (x) 10))
(exn/msg "expected identifier" (for/list (((x y) z)) 10))
(exn/msg "expected identifier" (for/list ([10 10]) 10))
(exn/msg "expected identifier" (for/list ([(+ 1 1) 10]) 10))
(exn/msg "unexpected term" (for/list ([x 1]) 10 10))
(exn/msg "expected comprehension clause" (for/list ([y 2] x) 10))
(exn/msg "expected identifier" (for/list ([y 2][(+ 1 1) 10]) 10))
(exn/msg "expected identifier" (for/list ([x 1] ([(x y) a z])) 10))
(exn/msg "expected identifier" (for/list ([x 1][10 x]) 10))


(exn/msg "expected structure type" (match 1 [(var x) x]))
