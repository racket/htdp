
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

(err/rt-test (1 2 3))
(err/rt-test (+) exn:application:arity?)

(define (f a) (a))
(test (void) f void)

(define (x) 10)
(test 10 x)
(define x (lambda () 11))
(test 11 x)

(define-struct a0 ())
(test #t a0? (make-a0))

(syntax-test #'begin)
(syntax-test #'(begin))
(syntax-test #'(begin (define x 10)))
(syntax-test #'(begin (define x 10) x))
(syntax-test #'(let () (begin (define x 10) x)))
(syntax-test #'(+ 1 (begin)))

(test 1 'begin (begin 1))
(test 2 'begin (begin 1 2))
(test 3 'begin (begin 1 2 3))

(syntax-test #'set!)
(syntax-test #'(set!))
(syntax-test #'(set! x))
(syntax-test #'(set! 1 2))
(syntax-test #'(set! x 2 3))

(set! x 'hello)
(test 'hello 'access-x x)
(test 18 'set! (local [(define x 12)]
		 (begin
		   (set! x 18)
		   x)))

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

(syntax-test #'shared)
(syntax-test #'(shared))
(syntax-test #'(shared ()))
(syntax-test #'(shared () 1 2))
(syntax-test #'(shared (x) 2))
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

(report-errs)

