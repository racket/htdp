
;; Basic checks for the beginner language. Error messages really
;; should be inspected manually.

;; Limitations of this test suite:
;;  - It doesn't check reader-level parameterization, such as use of quotes
;;  - It doesn't check format of printed results
;;  - It doesn't check the absence of MzScheme forms

(load-relative "loadtest.ss")

;; Don't need these:
(define no-extra-if-tests? #t)

;; After we require beginner, thunks no longer work:
(define (do-report-errs ignored)
  (report-errs))

(require (lib "beginner.ss" "lang"))

(syntax-test #'quote)
(syntax-test #''1)
(syntax-test #''"hello")
(syntax-test #''(1 2))
(syntax-test #'''a)

(syntax-test #'())
(syntax-test #'(1 2 3))
(syntax-test #'("hello" 1 2))
(syntax-test #'(+))

(syntax-test #'define)
(syntax-test #'(define))
(syntax-test #'(define x))
(syntax-test #'(define x 10 12))
(syntax-test #'(define (x) 10))
(syntax-test #'(define (x y) 10 12))
(syntax-test #'(define (10 y) 12))
(syntax-test #'(define ("x" y) 12))
(syntax-test #'(define (y 10) 12))
(syntax-test #'(define (y "x") 12))
(syntax-test #'(define (y z 10) 12))
(syntax-test #'(define x lambda))
(syntax-test #'(define x (lambda)))
(syntax-test #'(define x (lambda (x))))
(syntax-test #'(define x (lambda y)))
(syntax-test #'(define x (lambda y 10)))
(syntax-test #'(define x (lambda (10) 10)))
(syntax-test #'(define x (lambda (x 10) 10)))
(syntax-test #'(define x (lambda () 10)))
(syntax-test #'(define x (lambda (y) 10 11)))
(syntax-test #'(define x (lambda (y) (lambda (z) z))))

(syntax-test #'define-struct)
(syntax-test #'(define-struct))
(syntax-test #'(define-struct a))
(syntax-test #'(define-struct a (b) 10))
(syntax-test #'(define-struct 10 (b)))
(syntax-test #'(define-struct a b))
(syntax-test #'(define-struct a (10)))
(syntax-test #'(define-struct a (b 10)))
(syntax-test #'(define-struct (a) (b)))
(syntax-test #'(define-struct a ())) ; because constructor would be useless

(define x 5)
(define (f y) (+ x y))
(test 5 'lookup x)
(test 9 f 4)

(define-struct a1 (b))
(define-struct a3 (b c d))
(test #t a1? (make-a1 1))
(test #t a3? (make-a3 1 2 3))
(test #f a1? (make-a3 1 2 3))
(test #f a3? (make-a1 1))

(syntax-test #'cond)
(syntax-test #'(cond))
(syntax-test #'(cond 1))
(syntax-test #'(cond [#t 6] 2))
(syntax-test #'(cond [else 6] [#f 10]))
(syntax-test #'(cond [else 6] [else 10]))
(syntax-test #'(cond []))
(syntax-test #'(cond [1]))
(syntax-test #'(cond [1 2 3]))
(syntax-test #'(cond [1 2][]))
(syntax-test #'(cond [1 2][3 4 5]))

(test 17 'cond (cond [else 17]))
(test 18 'cond (cond [#t 18]))
(test 19 'cond (cond [(zero? 10) 0] [#t 19]))
(test 19 'cond (cond [(zero? 10) 0] [else 19]))

(err/rt-test (cond [#f 10]) exn:user?) ;; Should it be a different exception?
(err/rt-test (cond [1 10]))

(syntax-test #'if)
(syntax-test #'(if))
(syntax-test #'(if #t))
(syntax-test #'(if #t 1))
(syntax-test #'(if #t 1 2 3))

(arity-test + 2 -1)
(arity-test * 2 -1)
(arity-test / 2 -1)
(arity-test - 1 -1)

(err/rt-test (cons 1 2))
(err/rt-test (append (list 1) 2))

(do-report-errs #t)
