
;; Basic checks for the advanced language. See also
;;  beginner.ss

(load-relative "loadtest.ss")

;; Don't need these:
(define no-extra-if-tests? #t)

;; Check export names:
(require (lib "docprovide.ss" "syntax"))
(let ([docs (lookup-documentation '(lib "advanced.ss" "lang") 'procedures)])
  (for-each
   (lambda (row)
     (for-each
      (lambda (doc)
	(let ([v (dynamic-require '(lib "advanced.ss" "lang") (car doc))])
	  (when (and (procedure? v)
		     (not (eq? v call/cc)))
	    (test (car doc) object-name v))))
      (cdr row)))
   docs))

(require (lib "advanced.ss" "lang"))

(load-relative "beg-adv.ss")
(load-relative "bega-adv.ss")
(load-relative "intm-adv.ss")

(define (f6 a) (a))
(test (void) f6 void)

(define (x7) 10)
(test 10 x7)
(define x8 (lambda () 11))
(test 11 x8)

(syntax-test #'begin)
(syntax-test #'(begin))
(syntax-test #'(begin (define x 10)))
(syntax-test #'(begin (define x 10) x))
(syntax-test #'(let () (begin (define x 10) x)))
(syntax-test #'(+ 1 (begin)))

(test 1 'begin (begin 1))
(test 2 'begin (begin 1 2))
(test 3 'begin (begin 1 2 3))

(syntax-test #'begin0)
(syntax-test #'(begin0))

(test 1 'begin0 (begin0 1))
(test 2 'begin0 (begin0 2 1))
(test 3 'begin0 (begin0 3 2 1))

(syntax-test #'set!)
(syntax-test #'(set!))
(syntax-test #'(set! x))
(syntax-test #'(set! 1 2))
(syntax-test #'(set! x 2 3))
(syntax-test #'(set! set! 2))
(syntax-test #'(lambda (x) (set! x 2)))
(syntax-test #'(let ([x 5]) (lambda (x) (set! x 2))))

(set! x 'hello)
(test 'hello 'access-x x)
(test 18 'set! (local [(define x 12)]
		 (begin
		   (set! x 18)
		   x)))
(test 19 (lambda (x)
	   (local [(define x 12)]
		  (begin
		    (set! x 19)
		    x)))
      45)

(syntax-test #'delay)
(syntax-test #'(delay))
(syntax-test #'(delay 1 2))

(define d (delay (begin (set! x 89) 12)))
(test #t promise? d)
(test 12 force d)
(test 89 'access-x x)
(set! x 13)
(test 12 force d)
(test 13 'access-x x)

(syntax-test #'(let name))
(syntax-test #'(let name 10))
(syntax-test #'(let name ()))
(syntax-test #'(let name ([x]) 1))
(syntax-test #'(let name ([x 10] 2) 1))
(syntax-test #'(let name ([11 10]) 1))
(syntax-test #'(let name ([x 10]) 1 2))
(syntax-test #'(let name ([x 10][x 11]) 1))
(test 10 'lookup (let name () 10))
(test 1024 'loop (let loop ([n 10]) (if (zero? n) 1 (* 2 (loop (sub1 n))))))

(test 19 'lookup (recur empty-f () 19))

(syntax-test #'case)
(syntax-test #'(case))
(syntax-test #'(case 5))
(syntax-test #'(case 5 12))
(syntax-test #'(case 5 []))
(syntax-test #'(case 5 [5 10]))
(syntax-test #'(case 5 [(5) 10] 12))
(syntax-test #'(case 5 [(5)]))
(syntax-test #'(case 5 [(5) 12 13]))
(syntax-test #'(case 5 [("a") 10]))
(syntax-test #'(case 5 [() 10]))
(syntax-test #'(case 5 [(5 "a") 10]))
(syntax-test #'(case 5 [else 12][(5) 10]))
(syntax-test #'(case 5 [(5) 10][else 12][else 13]))

(test 'a 'case (case 5 [(5) 'a]))
(test 'b 'case (case 5 [(6) 'a][else 'b]))
(test 'c 'case (case 5 [(6 5) 'c][else 'b]))
(test 'd 'case (case 'hello [(6 5 hello) 'd][else 'b]))
(test 'd 'case (case 'hello [(no) 10][(6 5 hello) 'd][else 'b]))
(test 'cc 'case (case (+ 2 3) [(6 5) 'cc][else 'b]))

(syntax-test #'when)
(syntax-test #'(when))
(syntax-test #'(when 10))
(syntax-test #'(when 10 12 13))

(err/rt-test (when 1 2))

(test (void) 'when (when false 1))
(test 11 'when (when true 11))

(syntax-test #'unless)
(syntax-test #'(unless))
(syntax-test #'(unless 10))
(syntax-test #'(unless 10 12 13))

(err/rt-test (unless 1 2))

(test (void) 'unless (unless true 1))
(test 11 'unless (unless false 11))

(syntax-test #'shared)
(syntax-test #'(shared))
(syntax-test #'(shared ()))
(syntax-test #'(shared 1 2))
(syntax-test #'(shared () 1 2))
(syntax-test #'(shared (x) 2))
(syntax-test #'(shared ([]) 2))
(syntax-test #'(shared ([x]) 2))
(syntax-test #'(shared ([x 1 3]) 2))
(syntax-test #'(shared ([1 3]) 2))
(syntax-test #'(shared ([x 1][x 2]) 2))

(test 1 'shared (shared () 1))
(test 1 'shared (shared ([x 1]) x))
(test '(1) 'shared (shared ([x (cons 1 null)]) x))
(test 1 car (shared ([x (cons 1 x)]) x))
(test 1 cadr (shared ([x (cons 1 x)][y (cons 2 x)]) y))
(test 1 cadddr (shared ([x (cons 1 x)][y (cons 2 x)]) y))
(test #t (lambda (l) (eq? l (cdr l))) (shared ([x (cons 1 x)]) x))
(test #t (lambda (l) (eq? l (car l))) (shared ([x (list x x)]) x))
(test #t (lambda (l) (eq? l (cadr l))) (shared ([x (list x x)]) x))
(err/rt-test (shared ([x (cons 1 y)][y 5]) x))

(report-errs)

