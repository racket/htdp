
(test 1 'quote '1)
(test (list 'quote 1) 'quote ''1)
(test "Hello" 'quote '"Hello")
(test (list 1 2) 'quote '(1 2))
(test (list 1 (list 2 "hi")) 'quote '(1 (2 "hi")))

(test 1 'qq `1)
(test '(1 2) 'qq `(1 2))
(test 7 'qq `,(+ 3 4))
(test '(1 3) 'qq `(1 ,(+ 1 2)))
(test '(99 88 77) 'qq `(,(* 11 9) ,(* 11 `8) ,`,(* 11 7)))
(test '(1 2 3 4) 'qq `(1 ,@(list 2 3) 4))
(test '(quasiquote 11) 'qq ``11)
(test '(quasiquote (unquote 11)) 'qq ``,11)
(test '(quasiquote (unquote 22)) 'qq ``,,(* 11 2))
(test '(quasiquote ((unquote-splicing (22)))) 'qq ``(,@(,@(list (* 11 2)))))

(syntax-test #'quasiquote)
(syntax-test #'`unquote)
(syntax-test #'`unquote-splicing)
(syntax-test #'`(unquote-splicing 10))

(syntax-test #'unquote)
(syntax-test #'(unquote))
(syntax-test #'(unquote 10))

(syntax-test #'unquote-splicing)
(syntax-test #'(unquote-splicing (list 10)))
(syntax-test #'((unquote-splicing (list 10))))

(err/rt-test `(,@4))

(syntax-test #'(lambda (z z) 10))

(define f (lambda (y) (lambda (z) z)))
(test #t procedure? f)
(test 778 (lambda (x) 778) 'ignored)

(test values (lambda (f) (f f)) values)

(syntax-test #'local)
(syntax-test #'(local))
(syntax-test #'(local ()))
(syntax-test #'(local 1))
(syntax-test #'(local 1 1))
(syntax-test #'(local () 1 2))
(syntax-test #'(local [1] 1 2))
(syntax-test #'(local [(+ 1 2)] 1))
(syntax-test #'(local [(define x)] 1))
(syntax-test #'(local [(lambda (x) x)] 1))
(syntax-test #'(local [(define x 1) (define x 2)] 1))
(syntax-test #'(local [(define (x a) 12) (+ 1 2)] 1))

(err/rt-test (local [(define x y) (define y 5)] 10) exn:variable?)

(test 1 'local (local () 1))
(test 5 'local (local [(define y 5) (define x y)] x))
(test #t 'local (local [(define (even n) (if (zero? n) true (odd (sub1 n))))
			(define (odd n) (if (zero? n) false (even (sub1 n))))]
		       (even 100)))
(test 19 (local [(define (f x) (+ x 10))] f) 9)
(test 16 'local (local [(define (f x) (+ x 10))] (f 6)))

(syntax-test #'letrec)
(syntax-test #'(letrec))
(syntax-test #'(letrec ()))
(syntax-test #'(letrec 1 2))
(syntax-test #'(letrec 1 2 3))
(syntax-test #'(letrec (10) 1))
(syntax-test #'(letrec ([x]) 1))
(syntax-test #'(letrec ([x 2 3]) 1))
(syntax-test #'(letrec ([x 5] 10) 1))
(syntax-test #'(letrec ([1 5]) 1))
(syntax-test #'(letrec ([x 5]) 1 2))
(syntax-test #'(letrec ([x 5][x 6]) 1))

(err/rt-test (letrec ([x y] [y 5]) 10) exn:variable?)

(test 1 'letrec (letrec () 1))
(test 5 'letrec (letrec ([y 5][x y]) x))
(test #t 'letrec (letrec ([even (lambda (n) (if (zero? n) true (odd (sub1 n))))]
			  [odd (lambda (n) (if (zero? n) false (even (sub1 n))))])
		   (even 100)))
(test 19 (letrec ([f (lambda (x) (+ x 10))]) f) 9)
(test 16 'letrec (letrec ([f (lambda (x) (+ x 10))]) (f 6)))

(syntax-test #'let)
(syntax-test #'(let))
(syntax-test #'(let ()))
(syntax-test #'(let 1 2))
(syntax-test #'(let 1 2 3))
(syntax-test #'(let (10) 1))
(syntax-test #'(let ([x]) 1))
(syntax-test #'(let ([x 2 3]) 1))
(syntax-test #'(let ([x 5] 10) 1))
(syntax-test #'(let ([1 5]) 1))
(syntax-test #'(let ([x 5]) 1 2))
(syntax-test #'(let ([x 5][x 6]) 1))

(test 1 'let (let () 1))
(test 5 'let (let ([y 5]) (let ([x y]) x)))
(test 6 'let (let ([y 6]) (let ([y 10][x y]) x)))
(test 19 (let ([f (lambda (x) (+ x 10))]) f) 9)
(test 16 'let (let ([f (lambda (x) (+ x 10))]) (f 6)))

(test 7779 'time (time 7779))
(syntax-test #'(time (define x 5)))

