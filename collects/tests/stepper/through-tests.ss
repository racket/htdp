(module through-tests mzscheme
  (require (lib "private/shared.ss" "stepper")
           (lib "private/model.ss" "stepper")
           (lib "private/model-settings.ss" "stepper")
           (lib "private/highlight-placeholder.ss" "stepper")
           (lib "match.ss")
           "tests-common.ss")
  
  (reset-namespaces)
  
  (define (test-sequence namespace render-settings exp-str expected-steps)
    (printf "testing string: ~v\n" exp-str)
    (parameterize ([current-namespace namespace])
      (let* ([expanded-steps
              (map expand-test-spec expected-steps)]
             [receive-result
              (lambda (result)
                (if (compare-steps result (car expanded-steps))
                    (set! expanded-steps (cdr expanded-steps))
                    (printf "test-sequence: steps do not match.\ngiven: ~v\nexpected: ~v\n" result (car expanded-steps))))]
             [expand-in-namespace
              (lambda (sexp)
                (expand sexp))]
             [program-expander
              (lambda (init iter)
                (letrec ([input-port (open-input-string exp-str)]
                         [read-and-deliver
                          (lambda ()
                            (let ([new-exp (read input-port)])
                              (if (eof-object? new-exp)
                                  (iter new-exp void)
                                  (iter (expand-in-namespace new-exp) read-and-deliver))))])
                  (init)
                  (read-and-deliver)))])
        (go program-expander receive-result render-settings))))
  
  (define (lang-level-test-sequence ns rs)
    (lambda args
      (apply test-sequence ns rs args)))
  
  (define test-mz-sequence (lang-level-test-sequence mz-namespace fake-mz-render-settings))
  (define test-beginner-sequence (lang-level-test-sequence beginner-namespace fake-beginner-render-settings))
  (define test-beginner-wla-sequence (lang-level-test-sequence beginner-wla-namespace fake-beginner-wla-render-settings))
  (define test-intermediate-sequence (lang-level-test-sequence intermediate-namespace fake-intermediate-render-settings))
  (define test-intermediate/lambda-sequence (lang-level-test-sequence intermediate/lambda-namespace fake-intermediate/lambda-render-settings))
  
  
  (define (compare-steps actual expected)
    (case (car expected)
      [(before-after) (and (before-after-result? actual)
                           (andmap equal? 
                                   (map (lambda (fn) (fn actual))
                                        (list before-after-result-exp
                                              before-after-result-redex
                                              before-after-result-post-exp
                                              before-after-result-reduct))
                                   (cdr expected)))]
      [(finished) (let ([finished-exps (finished-result-finished-exprs actual)]
                        [expected-exps (cadr expected)]) 
                    (and 
                     (>= (length finished-exps) (length expected-exps))
                     (andmap equal?
                             (list-tail finished-exps (- (length finished-exps) (length expected-exps)))
                             expected-exps)))]
      
      [(error) (equal? (cadr expected) (error-result-err-msg actual))]
      
      [else (printf "compare-steps: unexpected expected step type: ~v\n" (car expected))]))
  
  
  (define (expand-test-spec spec)
    (match spec
      [`(before-after ,a ,b same ,d)
       (expand-test-spec 
        `(before-after ,a ,b ,a ,d))]
      [else spec]))
  
  ;;;;;;;;;;;;;
  ;;
  ;;  mz tests
  ;;
  ;;;;;;;;;;;;;
  
  (define h-p highlight-placeholder)
  
  (test-mz-sequence "(for-each (lambda (x) x) '(1 2 3))"
                    `((before-after (,h-p) ((for-each (lambda (x) x) `(1 2 3))) ((... ,h-p ...)) (1))
                      (before-after (,h-p) (...) ((... ,h-p ...)) (2))
                      (before-after (,h-p) (...) ((... ,h-p ...)) (3))
                      (before-after (,h-p) (...) (,h-p) ((void)))
                      (finished ((void)))))
  
  (test-mz-sequence "(+ 3 4)"
                    `((before-after (,h-p) ((+ 3 4)) (,h-p) (7))
                      (finished (7))))
  
  (test-mz-sequence "((lambda (x) (+ x 3)) 4)"
                    `((before-after (,h-p) (((lambda (x) (+ x 3)) 4))
				    (,h-p) ((+ 4 3)))
                      (before-after (,h-p) ((+ 4 3))
				    (,h-p) (7))
		      (finished (7))))
  
  (test-mz-sequence "(if 3 4 5)"
                    `((before-after (,h-p) ((if 3 4 5))
				    (,h-p) (4))
		      (finished (4))))
  
  (test-beginner-sequence "(if (if true false true) false true)"
                          `((before-after ((if ,h-p false true)) ((if true false true))
					  ((if ,h-p false true)) (false))
                            (before-after (,h-p) ((if false false true))
					  (,h-p) (true))
			    (finished (true))))
  
  (test-mz-sequence "((lambda (x) x) 3)"
                    `((before-after (,h-p) (((lambda (x) x) 3))
				    (,h-p) (3))
		      (finished (3))))
  

;  (test-mz-sequence "((lambda (x) x) (begin (+ 3 4) (+ 4 5)))"
;		    `((before-after ((begin ,h-p (+ 4 5))) ((+ 3 4))
;				    ((begin ,h-p (+ 4 5))) (7))
;		      (before-after (,h-p) ((begin 7 (+ 4 5)))
;				    (,h-p) ((+ 4 5)))
;                      (before-after (,h-p) ((+ 4 5))
;				    (,h-p) (9))
;		      (finished (9))))
  
  (test-mz-sequence "((lambda (a) (lambda (b) (+ a b))) 14)"
                    `((before-after (,h-p) (((lambda (a) (lambda (b) (+ a b))) 14))
				    (,h-p) ((lambda (b) (+ 14 b))))
		      (finished ((lambda (b) (+ 14 b))))))
  
  (test-mz-sequence "((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)"
                    `((before-after (,h-p) (((case-lambda ((a) 3) ((b c) (+ b c))) 5 6))
				    (,h-p) ((+ 5 6)))
                      (before-after (,h-p) ((+ 5 6))
				    (,h-p) (11))
		      (finished (11))))
  
  (test-mz-sequence "(if 3 4)"
                    `((before-after (,h-p) ((if 3 4))
				    (,h-p) (4))
		      (finished (4))))
  
;  (test-mz-sequence "(let ([a 3]) 4)"
;		    `((before-after (,h-p) ((let-values ([(a) 3]) 4))
;				    same (4)))) 
;  
;  (test-mz-sequence "(let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))"
;                    `((before-after ((let-values ([(a) ,h-p] [(b) (+ 9 20)]) (+ a b))) ((+ 4 5))
;				    same (9))
;		      (before-after ((let-values ([(a) 9] [(b) ,h-p]) (+ a b))) ((+ 9 20))
;				    same (29))
;		      (before-after (,h-p) ((let-values ([(a) 9] [(b) 29]) (+ a b)))
;				    same (+ 9 29))
;		      (before-after (,h-p) ((+ 9 29))
;				    same (38))
;		      (finished (38))))
  
  
  ;(test-mz-sequence "((call-with-current-continuation call-with-current-continuation) (call-with-current-continuation call-with-current-continuation))"
  ;                  `((before-after ((,h-p (call-with-current-continuation call-with-current-continuation))) ((call-with-current-continuation call-with-current-continuation))
  ;                    ((,h-p (call-with-current-continuation call-with-current-continuation))) ((lambda args ...)))
  ;                    (before-after (((lambda args ...) ,h-p)) ((call-with-current-continuation call-with-current-continuation))
  ;                    (((lambda args ...) ,h-p)) ((lambda args ...)))))
  
  ;(test-mz-sequence '(begin (define g 3) g)
  ;                  `((before-after (,h-p) (g)
  ;                    (,h-p) 3)))
  
  ;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))
  
  (test-beginner-sequence "(define a (+ 3 4))"
                          `((before-after ((define a ,h-p)) ((+ 3 4))
                            ((define a ,h-p)) (7))
			    (finished ((define a 7)))))
  
  (test-beginner-sequence "(+ 4 129)"
                          `((before-after (,h-p) ((+ 4 129))
					  same (133))
			    (finished (133))))
  
  (test-beginner-sequence "(if true 3 4)"
                          `((before-after (,h-p) ((if true 3 4))
					  (,h-p) (3))
			    (finished (3))))

  ;;;;;;;;;;;;;
  ;;
  ;;  OR / AND
  ;;
  ;;;;;;;;;;;;;
  
  (test-beginner-sequence "(or false true false)"
                          `((before-after (,h-p) ((or false true false))
					  (,h-p) ((or true false)))
                            (before-after (,h-p) ((or true false))
					  (,h-p) (true))
			    (finished (true))))
  
  (test-beginner-sequence "(and true false true)"
                          `((before-after (,h-p) ((and true false true))
					  (,h-p) ((and false true)))
                            (before-after (,h-p) ((and false true))
					  (,h-p) (false))
			    (finished (false))))
  
(test-beginner-sequence "(define (a2 x) x) (a2 4)"
                          `((finished ((define (a2 x) x)))
			    (before-after (,h-p) ((a2 4))
					  (,h-p) (4))
			    (finished (4))))
  
  (test-beginner-sequence "(define (a3 x) (if true x x)) (a3 false)"
                          `((finished ((define (a3 x) (if true x x)))) 
			    (before-after (,h-p) ((a3 false))
					  (,h-p) ((if true false false)))
                            (before-after (,h-p) ((if true false false))
					  (,h-p) (false))
			    (finished (false))))
  
  (test-beginner-sequence "(define (b2 x) (and true x)) (b2 false)"
                          `((finished ((define (b2 x) (and true x)))) 
			    (before-after (,h-p) ((b2 false))
					  (,h-p) ((and true false)))
                            (before-after (,h-p) ((and true false))
                                          (,h-p) (false))
			    (finished (false))))
  
  (test-beginner-sequence "(define a1 true)(define (b1 x) (and a1 true x)) (b1 false)"
                          `((finished ((define a1 true)
                                       (define (b1 x) (and a1 true x))))
                            (before-after (,h-p) ((b1 false))
                                          (,h-p) ((and a1 true false)))
                            (before-after ((and ,h-p true false)) (a1)
                                          ((and ,h-p true false)) (true))
                            (before-after (,h-p) ((and true true false))
                                          (,h-p) ((and true false)))
                            (before-after (,h-p) ((and true false))
                                          (,h-p) (false))
                            (finished (false))))
  
  ;;;;;;;;;;;;;
  ;;
  ;;  COND
  ;;
  ;;;;;;;;;;;;;
  
  
  (test-beginner-sequence "(cond [false 4] [false 5] [true 3])"
                          `((before-after (,h-p) ((cond (false 4) (false 5) (true 3)))
					  (,h-p) ((cond (false 5) (true 3))))
                            (before-after (,h-p) ((cond (false 5) (true 3)))
					  (,h-p) ((cond (true 3))))
                            (before-after (,h-p) ((cond (true 3)))
					  (,h-p) (3))
			    (finished (3))))
  
  (test-beginner-sequence "(cond [false 4] [else 9])"
                          `((before-after (,h-p) ((cond [false 4] [else 9]))
					  (,h-p) ((cond [else 9])))
                            (before-after (,h-p) ((cond [else 9]))
					  (,h-p) (9))
			    (finished (9))))
  
  (test-beginner-sequence "(cond [true 3] [else (and true true)])"
                          `((before-after (,h-p) ((cond (true 3) (else (and true true))))
					  (,h-p) (3))
			    (finished (3))))
  
  
  (test-beginner-sequence "(cond)"
			  `((error "cond should have more clauses")))
  
  (test-beginner-sequence "(cond [else 3])"
                          `((before-after (,h-p) ((cond (else 3)))
					  (,h-p) (3))
			    (finished (3))))
  
  (test-beginner-sequence "(cond [else (cond [else 3])])"
                          `((before-after (,h-p) ((cond (else (cond (else 3)))))
					  (,h-p) ((cond (else 3))))
                            (before-after (,h-p) ((cond (else 3)))
					  (,h-p) (3))
			    (finished (3))))
  
  ; reconstruct can't handle begin
  (test-mz-sequence "(cond [#f 3 4] [#t (+ 3 4) (+ 4 9)])"
                    `((before-after (,h-p) ((cond (#f 3 4) (#t (+ 3 4) (+ 4 9))))
				    (,h-p) ((cond (#t (+ 3 4) (+ 4 9)))))
                      (before-after (,h-p) ((cond (#t (+ 3 4) (+ 4 9))))
				    (,h-p) (begin (+ 3 4) (+ 4 9)))
                      (before-after ((begin ,h-p (+ 4 9))) ((+ 3 4))
				    ((begin ,h-p (+ 4 9)))  (7))
                      (before-after (,h-p) ((begin 7 (+ 4 9)))
				    (,h-p) ((+ 4 9)))
                      (before-after (,h-p) ((+ 4 9))
				    (,h-p) (13))
		      (finished (13))))
  
  
  
  (test-beginner-sequence "(cond [false 3] [else (cond [true 4])])"
                          `((before-after (,h-p) ((cond (false 3) (else (cond (true 4)))))
					  (,h-p) ((cond (else (cond (true 4))))))
                            (before-after (,h-p) ((cond (else (cond (true 4)))))
					  (,h-p) ((cond (true 4))))
                            (before-after (,h-p) ((cond (true 4)))
					  (,h-p) (4))
			    (finished (4))))
  

  
  
  
  
  (test-intermediate-sequence "(define a4 +) a4"
                              `((before-after (,h-p) (a4)
                                              (,h-p) (+))
                                (finished (+))))
  
  (test-intermediate-sequence "(define (f123 x) (+ x 13)) f123"
                              `((finished ((define (f123 x) (+ x 13))
                                           f123))))
  
  (test-beginner-sequence "(define (b x) (+ x 13)) (b 9)"
                          `((finished ((define (b x) (+ x 13))))
                            (before-after (,h-p) ((b 9))
                                          (,h-p) ((+ 9 13)))
                            (before-after (,h-p) ((+ 9 13))
                                          (,h-p) (22))
                            (finished (22))))

  
  (test-beginner-sequence "(define-struct mamba (rhythm tempo)) (mamba-rhythm (make-mamba 24 2))"
                          `((finished ((define-struct mamba (rhythm tempo))))
                            (before-after (,h-p) ((mamba-rhythm (make-mamba 24 2)))
                                          (,h-p) (24))
                            (finished (24))))
  
  (test-beginner-sequence "(define a5 (lambda (a5) (+ a5 13))) (a5 23)"
                          `((finished ((define a5 (lambda (a5) (+ a5 13)))))
                            (before-after (,h-p) ((a5 23))
                                          (,h-p) ((+ 23 13)))
                            (before-after (,h-p) ((+ 23 13))
                                          (,h-p) (36))
                            (finished (36))))
  
  (test-beginner-sequence "(define c1 false) (define (d2 x) (or c1 false x)) (d2 false)"
                          `((finished ((define c1 false)
                                       (define (d2 x) (or c1 false x))))
                            (before-after (,h-p) ((d2 false))
                                          (,h-p) ((or c1 false false)))
                            (before-after ((or ,h-p false false)) (c1)
                                          ((or ,h-p false false)) (false))
                            (before-after (,h-p) ((or false false false))
                                          (,h-p) ((or false false)))
                            (before-after (,h-p) ((or false false))
                                          (,h-p) (false))
                            (finished (false))))
  
  (test-beginner-sequence "(define (silly-choice str)
                             (string-append str (if false str str) str))
  (silly-choice \"family\")"
                          `((finished ((define (silly-choice str)
                                         (string-append str (if false str str) str))))
                            (before-after (,h-p) ((silly-choice "family"))
                                          (,h-p) ((string-append "family" (if false "family" "family") "family")))
                            (before-after ((string-append "family" ,h-p "family")) ((if false "family" "family"))
                                          ((string-append "family" ,h-p "family")) ("family"))
                            (before-after (,h-p) ((string-append "family" "family" "family"))
                                          (,h-p) ("familyfamilyfamily"))
                            (finished ("familyfamilyfamily"))))
  
  (test-beginner-sequence "(define (f x) (+ (g x) 10)) (define (g x) (- x 22)) (f 13)"
                          `((finished ((define (f x) (+ (g x) 10)) (define (g x) (- x 22))))
                            (before-after (,h-p) ((f 13)) same ((+ (g 13) 10)))
                            (before-after ((+ ,h-p 10)) ((g 13)) same ((- 13 22)))
                            (before-after ((+ ,h-p 10)) ((- 13 22)) same (-9))
                            (before-after (,h-p) ((+ -9 10)) same (1))
                            (finished (1))))
  
  (test-beginner-sequence "(define (f2 x) (+ (g2 x) 10))"
                          `((finished ((define (f2 x) (+ (g2 x) 10))))))
  
  
  (test-beginner-sequence "(cons 1 2)" 
                          `((error "second argument to cons should be list")))
  
  (test-beginner-sequence "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
                          `((finished ((cons 3 (cons 1 empty))))
                            (before-after (,h-p) ((list 1 2 3)) same ((cons 1 (cons 2 (cons 3 empty)))))
                            (finished ((cons 1 (cons 2 (cons 3 empty))) (define-struct aa (b)) (make-aa 3)))))
  
  (test-beginner-sequence "(define a11 4)"
                          `((finished ((define a11 4)))))
  
  (test-mz-sequence "(map (lambda (x) x) (list 3 4 5))"
                    `((before-after ((map (lambda (x) x) ,h-p)) ((list 3 4 5))
                                    same (`( 3 4 5)))
                      (before-after (,h-p) ((map (lambda (x) x) `(3 4 5)))
                                    ((... ,h-p ...)) (3))
                      (before-after (...) ()
                                    ((... ,h-p ...)) (4))
                      (before-after (...) ()
                                    ((... ,h-p ...)) (5))
                      (before-after (...) ()
                                    (,h-p) (`(3 4 5)))
                      (finished (`(3 4 5)))))
  
  (test-beginner-wla-sequence "'(3 4 5)"
                              `((finished ((list 3 4 5)))))
  
  ; note: we currently punt on trying to unwind quasiquote.
  
  (test-beginner-wla-sequence "`(3 4 ,(+ 4 5))"
                              `((before-after ((cons 3 (cons 4 (cons ,h-p empty)))) ((+ 4 5))
                                              ((cons 3 (cons 4 (cons ,h-p empty)))) (9))
                                (before-after ((cons 3 (cons 4 ,h-p))) ((cons 9 empty))
                                              ((cons 3 (cons 4 ,h-p))) ((list 9)))
                                (before-after ((cons 3 ,h-p)) ((cons 4 (list 9)))
                                              ((cons 3 ,h-p)) ((list 4 9)))
                                (before-after (,h-p) ((cons 3 (list 4 9)))
                                              (,h-p) ((list 3 4 9)))
                                (finished ((list 3 4 9)))))
  
  (test-beginner-wla-sequence "`(3 ,@(list (+ 3 4) 5) 6)"
                              `((before-after ((cons 3 (append (list ,h-p 5) (cons 6 empty)))) ((+ 3 4)) same (7))
                                (before-after ((cons 3 (append (list 7 5) ,h-p))) ((cons 6 empty)) same ((list 6)))
                                (before-after ((cons 3 ,h-p)) ((append (list 7 5) (list 6))) same ((list 7 5 6)))
                                (before-after (,h-p) ((cons 3 (list 7 5 6))) same ((list 3 7 5 6)))
                                (finished ((list 3 7 5 6)))))
  
  (test-intermediate-sequence "(local () (+ 3 4))"
                              `((before-after (,h-p) ((local () (+ 3 4)))
                                              (,h-p) ((+ 3 4)))
                                (before-after (,h-p) ((+ 3 4))
                                              (,h-p) (7))
                                (finished (7))))
  
  (test-intermediate-sequence "(local ((define (a x) (+ x 9))) (a 6))"
                              `((())))
  
  (test-intermediate-sequence "(local ((define (a x) (+ x 13))) a)"
                              `((before-after ())))

  (test-intermediate-sequence "(local ((define (a x) (+ x 9)) (define b a)) (b 1))")
  
  
  ;;;;;;;;;;;;;
  ;;
  ;;  TEACHPACK TESTS
  ;;
  ;;;;;;;;;;;;;
  
  (require (lib "mred.ss" "mred"))
  
  (define tp-namespace
    (let ([ns (current-namespace)]
          [mred-name ((current-module-name-resolver) '(lib "mred.ss" "mred") #f #f)]
          [new-namespace (make-namespace 'empty)])
      (parameterize ([current-namespace new-namespace])
        (namespace-attach-module ns 'mzscheme)
        (namespace-attach-module ns mred-name)
        (namespace-require '(lib "htdp-beginner.ss" "lang"))
        (namespace-require '(lib "guess.ss" "htdp"))
        new-namespace)))
  
  ; uses set-render-settings!
  ;(reconstruct:set-render-settings! fake-beginner-render-settings)
  ;(test-sequence "(define (check-guess guess target) 'TooSmall) (guess-with-gui check-guess)"
  ;               `((before-after (,h-p) ((guess-with-gui check-guess)))
  ;                 ((,h-p) (true)))
  ;               `((define (check-guess guess target) 'toosmall) true)
  ;               tp-namespace)
  
  
   )