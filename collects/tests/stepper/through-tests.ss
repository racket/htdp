(module through-tests mzscheme
  (require (lib "private/shared.ss" "stepper")
           (lib "private/model.ss" "stepper")
           (lib "private/model-settings.ss" "stepper")
           (lib "private/highlight-placeholder.ss" "stepper")
           (lib "match.ss")
           (lib "sexp-diff.scm" "tests" "utils")
           "module-elaborator.ss")
  
  (define test-directory (build-path (collection-path "mzlib")  'up 'up))
  
  (define (stream-ify expr-list iter)
    (lambda ()
      (if (null? expr-list)
          (iter eof void)
          (iter (car expr-list) (stream-ify (cdr expr-list) iter)))))
  
  (define (test-sequence namespace-spec render-settings exp-str expected-steps)
     (let ([filename (build-path test-directory "stepper-test")])
       (call-with-output-file filename
         (lambda (port)
           (fprintf port "~a" exp-str))
         'truncate)
       (printf "testing string: ~v\n" exp-str)
       (let* ([current-error-display-handler (error-display-handler)]) 
         (let/ec escape
           (parameterize ([error-escape-handler (lambda () (escape (void)))])
             (let* ([all-steps
                     (append expected-steps 
                             '((finished-stepping)))]
                    [receive-result
                     (lambda (result)
                       (if (null? all-steps)
                           (printf "test-sequence: ran out of expected steps. Given result: ~v\n" result)
                           (begin
                             (unless (compare-steps result (car all-steps))
                               (printf "test-sequence: steps do not match.\ngiven: ~v\nexpected: ~v\n" result (car all-steps)))
                             (set! all-steps (cdr all-steps)))))]
                    [program-expander
                     (lambda (init iter)
                       (call-with-input-file filename
                         (lambda (input-port)
                           (let* ([exps (let read-loop ()
                                          (let ([expr (read-syntax filename input-port)])
                                            (if (eof-object? expr)
                                                null
                                                (cons expr (read-loop)))))]
                                  [exprs (wrap-in-module exps namespace-spec)])
                             (init)
                             ((stream-ify exprs iter))))))])
               (go program-expander receive-result render-settings)
               (error-display-handler current-error-display-handler)))))))
  
  (define (lang-level-test-sequence namespace-spec rs)
    (lambda args
      (apply test-sequence namespace-spec rs args)))
  
  (define (make-multi-level-test-sequence level-fns)
    (lambda args
      (for-each (lambda (fn) (apply fn args)) level-fns)))
  
  (define test-mz-sequence (lang-level-test-sequence 'mzscheme fake-mz-render-settings))
  (define test-beginner-sequence (lang-level-test-sequence `(lib "htdp-beginner.ss" "lang") fake-beginner-render-settings))
  (define test-beginner-wla-sequence (lang-level-test-sequence `(lib "htdp-beginner-abbr.ss" "lang") fake-beginner-wla-render-settings))
  (define test-intermediate-sequence (lang-level-test-sequence `(lib "htdp-intermediate.ss" "lang") fake-intermediate-render-settings))
  (define test-intermediate/lambda-sequence (lang-level-test-sequence `(lib "htdp-intermediate-lambda.ss" "lang") fake-intermediate/lambda-render-settings))
  
  (define test-upto-int/lam (make-multi-level-test-sequence (list test-beginner-sequence
                                                                  test-beginner-wla-sequence
                                                                  test-intermediate-sequence
                                                                  test-intermediate/lambda-sequence)))
  
  (define test-bwla-to-int/lam (make-multi-level-test-sequence (list test-beginner-wla-sequence
                                                                     test-intermediate-sequence
                                                                     test-intermediate/lambda-sequence)))
  
  ; mutate these to values you want to examine in the repl:
  (define bell-jar-specimen-1 #f)
  (define bell-jar-specimen-2 #f)
  
  ; (-> step-result? sexp? boolean?)
  (define (compare-steps actual expected)
    (match expected
      [`(before-after ,before-contexts ,before-in-hole same ,after-in-hole)
       (compare-steps actual `(before-after ,before-contexts ,before-in-hole ,before-contexts ,after-in-hole))]
      [`(before-after ,before-contexts ,before-in-hole ,after-contexts ,after-in-hole)
       (and (before-after-result? actual)
            (andmap (lambda (fn expected) (noisy-equal? (fn actual) expected))
                    (list before-after-result-exp before-after-result-redex before-after-result-post-exp before-after-result-reduct)
                    (list before-contexts before-in-hole after-contexts after-in-hole)))]
      [`(before-after-waiting ,before-contexts ,before-in-hole ,after-contexts ,after-in-hole ,waiting)
       (and (before-after-result? actual)
            (and (noisy-equal? (before-after-result-after-exprs actual) waiting)
                 (compare-steps actual `(before-after ,before-contexts ,before-in-hole ,after-contexts ,after-in-hole))))]
      [`(before-after-finished ,finished-exprs . ,rest)
       (and (before-after-result? actual)
            (compare-finished (before-after-result-finished-exprs actual) finished-exprs)
            (compare-steps actual `(before-after ,@rest)))]
      [`(before-after-finished-waiting ,finished-exprs . ,rest)
       (and (before-after-result? actual)
            (compare-finished (before-after-result-finished-exprs actual) finished-exprs)
            (compare-steps actual `(before-after-waiting ,@rest)))]
      [`(finished ,finished-exprs)
       (and (finished-result? actual)
            (compare-finished (finished-result-finished-exprs actual) finished-exprs))]
      [`(error ,err-msg)
       (and (error-result? actual)
            (equal? err-msg (error-result-err-msg actual)))]
      [`(before-error ,before-contexts ,before-in-hole ,err-msg)
       (and (before-error-result? actual)
            (andmap (lambda (fn expected) (noisy-equal? (fn actual) expected))
                    (list before-error-result-exp before-error-result-redex before-error-result-err-msg)
                    (list before-contexts before-in-hole err-msg)))]
      [`(finished-stepping) (finished-stepping? actual)]
      [else 
       (begin (printf "compare-steps: unexpected expected step type: ~v\n" expected)
              #f)]))
  
  ; noisy-equal? : (any any . -> . boolean)
  ; like equal?, but prints a noisy error message
  (define (noisy-equal? a b)
    (if (equal? a b)
        #t
        (begin (printf "~e is not equal? to ~e\nhere's the diff: ~e\n" a b (sexp-diff a b))
               #f)))
   
  ; (-> (listof sexp) (listof sexp) boolean?)
  (define (compare-finished finished-exps expected-exps)
    (and 
     (>= (length finished-exps) (length expected-exps))
     (andmap (lambda (x y) (if (equal? x y)
                               #t
                               (begin (printf "~e is not equal? to ~e\nhere's the diff: ~e\n" x y (sexp-diff x y))
                                      #f)))
             (list-tail finished-exps (- (length finished-exps) (length expected-exps)))
             expected-exps)))
 
  
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
  
  (test-upto-int/lam "(if (if true false true) false true)"
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
  
  ;(test-mz-sequence "((call-with-current-continuation call-with-current-continuation) (call-with-current-continuation call-with-current-continuation))"
  ;                  `((before-after ((,h-p (call-with-current-continuation call-with-current-continuation))) ((call-with-current-continuation call-with-current-continuation))
  ;                    ((,h-p (call-with-current-continuation call-with-current-continuation))) ((lambda args ...)))
  ;                    (before-after (((lambda args ...) ,h-p)) ((call-with-current-continuation call-with-current-continuation))
  ;                    (((lambda args ...) ,h-p)) ((lambda args ...)))))
  
  ;(test-mz-sequence '(begin (define g 3) g)
  ;                  `((before-after (,h-p) (g)
  ;                    (,h-p) 3)))
  
  ;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))
  
  (test-upto-int/lam "(define a (+ 3 4))"
                     `((before-after ((define a ,h-p)) ((+ 3 4))
                                     ((define a ,h-p)) (7))
                       (finished ((define a 7)))))
  
  (test-upto-int/lam "(+ 4 129)" 
                     `((before-after (,h-p) ((+ 4 129))
                                     same (133))
                       (finished (133))))
  
  (test-upto-int/lam "(if true 3 4)"
                     `((before-after (,h-p) ((if true 3 4))
                                     (,h-p) (3))
                       (finished (3))))
  
 
  (test-upto-int/lam "(define (a2 x) x) (a2 4)"
                     `((before-after-finished ((define (a2 x) x)) 
                                              (,h-p) ((a2 4)) same (4))
                       (finished (4))))
  
  (test-upto-int/lam "(define (a3 x) (if true x x)) (a3 false)"
                     `((before-after-finished ((define (a3 x) (if true x x)))
                                              (,h-p) ((a3 false)) same ((if true false false)))
                       (before-after (,h-p) ((if true false false)) same (false))
                       (finished (false))))
  
  (test-intermediate-sequence "(define (a12 x) (+ x 9)) (define b12 a12) (b12 12)"
                              `((before-after-finished ((define (a12 x) (+ x 9)) (define b12 a12))
                                                       ((,h-p 12)) (b12) same (a12))
                                (before-after (,h-p) ((a12 12)) same ((+ 12 9)))
                                (before-after (,h-p) ((+ 12 9)) same (21))
                                (finished (21))))
  
  

  ;;;;;;;;;;;;
  ;;
  ;;  OR / AND
  ;;
  ;;;;;;;;;;;;;.
  
  (test-upto-int/lam "(or false true false)"
                     `((before-after (,h-p) ((or false true false))
                                     (,h-p) (true))
                       (finished (true))))
  
  (test-upto-int/lam "(and true false true)"
                     `((before-after (,h-p) ((and true false true))
                                     (,h-p) (false))
                       (finished (false))))
   
  (test-upto-int/lam "(and true (if true true false))"
                     `((before-after ((and true ,h-p)) ((if true true false)) same (true))
                       (before-after (,h-p) ((and true true)) same (true))
                       (finished (true))))
  
  (test-upto-int/lam "(define (b2 x) (and true x)) (b2 false)"
                     `((before-after-finished ((define (b2 x) (and true x)))
                                              (,h-p) ((b2 false)) same ((and true false)))
                       (before-after (,h-p) ((and true false)) same (false))
                       (finished (false))))
  
  (test-upto-int/lam "(define a1 true)(define (b1 x) (and a1 true x)) (b1 false)"
                     `((before-after-finished ((define a1 true)
                                               (define (b1 x) (and a1 true x))) 
                                              (,h-p) ((b1 false)) same ((and a1 true false)))
                       (before-after ((and ,h-p true false)) (a1) same (true))
                       (before-after (,h-p) ((and true true false)) same (false))
                       (finished (false))))
  
  (test-upto-int/lam "(and true 1)"
                     `((before-error (,h-p) ((and true 1)) "and: question result is not true or false: 1")))
  
  ;;;;;;;;;;;;;
  ;;
  ;;  COND
  ;;
  ;;;;;;;;;;;;;
  
  
  (test-upto-int/lam "(cond [false 4] [false 5] [true 3])"
                     `((before-after (,h-p) ((cond (false 4) (false 5) (true 3)))
                                     (,h-p) ((cond (false 5) (true 3))))
                       (before-after (,h-p) ((cond (false 5) (true 3)))
                                     (,h-p) ((cond (true 3))))
                       (before-after (,h-p) ((cond (true 3)))
                                     (,h-p) (3))
                       (finished (3))))
  
  (test-upto-int/lam "(cond [false 4] [else 9])"
                     `((before-after (,h-p) ((cond [false 4] [else 9]))
                                     (,h-p) ((cond [else 9])))
                       (before-after (,h-p) ((cond [else 9]))
                                     (,h-p) (9))
                       (finished (9))))
  
  (test-upto-int/lam "(cond [true 3] [else (and true true)])"
                     `((before-after (,h-p) ((cond (true 3) (else (and true true))))
                                     (,h-p) (3))
                       (finished (3))))
  
  
  (test-upto-int/lam "(cond)"
                     `((error "cond: expected a question--answer clause after `cond', but nothing's there")))
  
  (test-upto-int/lam "(cond [else 3])"
                     `((before-after (,h-p) ((cond (else 3)))
                                     (,h-p) (3))
                       (finished (3))))
  
  (test-upto-int/lam "(cond [else (cond [else 3])])"
                     `((before-after (,h-p) ((cond (else (cond (else 3)))))
                                     (,h-p) ((cond (else 3))))
                       (before-after (,h-p) ((cond (else 3)))
                                     (,h-p) (3))
                       (finished (3))))
  
  ; reconstruct can't handle begin
  ;  (test-mz-sequence "(cond [#f 3 4] [#t (+ 3 4) (+ 4 9)])"
  ;                    `((before-after (,h-p) ((cond (#f 3 4) (#t (+ 3 4) (+ 4 9))))
  ;				    (,h-p) ((cond (#t (+ 3 4) (+ 4 9)))))
  ;                      (before-after (,h-p) ((cond (#t (+ 3 4) (+ 4 9))))
  ;				    (,h-p) (begin (+ 3 4) (+ 4 9)))
  ;                      (before-after ((begin ,h-p (+ 4 9))) ((+ 3 4))
  ;				    ((begin ,h-p (+ 4 9)))  (7))
  ;                      (before-after (,h-p) ((begin 7 (+ 4 9)))
  ;				    (,h-p) ((+ 4 9)))
  ;                      (before-after (,h-p) ((+ 4 9))
  ;				    (,h-p) (13))
  ;		      (finished (13))))
  
  
  
  (test-upto-int/lam "(cond [false 3] [else (cond [true 4])])"
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
  
  (test-upto-int/lam "(define (b x) (+ x 13)) (b 9)"
                     `((before-after-finished ((define (b x) (+ x 13)))
                                              (,h-p) ((b 9)) same ((+ 9 13)))
                       (before-after (,h-p) ((+ 9 13)) same (22))
                       (finished (22))))
  
  
  (test-upto-int/lam "(define-struct mamba (rhythm tempo)) (mamba-rhythm (make-mamba 24 2))"
                     `((before-after-finished ((define-struct mamba (rhythm tempo)))
                                              (,h-p) ((mamba-rhythm (make-mamba 24 2))) same (24))
                       (finished (24))))
  
  (test-upto-int/lam "(define a5 (lambda (a5) (+ a5 13))) (a5 23)"
                     `((before-after-finished ((define a5 (lambda (a5) (+ a5 13))))
                                              (,h-p) ((a5 23)) same ((+ 23 13)))
                       (before-after (,h-p) ((+ 23 13)) same (36))
                       (finished (36))))
  
  (test-upto-int/lam "(define c1 false) (define (d2 x) (or c1 false x)) (d2 false)"
                     `((before-after-finished ((define c1 false)
                                               (define (d2 x) (or c1 false x)))
                                              (,h-p) ((d2 false)) same ((or c1 false false)))
                       (before-after ((or ,h-p false false)) (c1) same (false))
                       (before-after (,h-p) ((or false false false)) same (false))
                       (finished (false))))
  
  (test-upto-int/lam "(define (silly-choice str)
                        (string-append str (if false str str) str))
  (silly-choice \"family\")"
                     `((before-after-finished ((define (silly-choice str)
                                                 (string-append str (if false str str) str)))
                                              (,h-p) ((silly-choice "family")) 
                                              same ((string-append "family" (if false "family" "family") "family")))
                       (before-after ((string-append "family" ,h-p "family")) ((if false "family" "family"))
                                     same ("family"))
                       (before-after (,h-p) ((string-append "family" "family" "family"))
                                     same ("familyfamilyfamily"))
                       (finished ("familyfamilyfamily"))))
  
  (test-upto-int/lam "(define (f x) (+ (g x) 10)) (define (g x) (- x 22)) (f 13)"
                     `((before-after-finished ((define (f x) (+ (g x) 10)) (define (g x) (- x 22)))
                                              (,h-p) ((f 13)) same ((+ (g 13) 10)))
                       (before-after ((+ ,h-p 10)) ((g 13)) same ((- 13 22)))
                       (before-after ((+ ,h-p 10)) ((- 13 22)) same (-9))
                       (before-after (,h-p) ((+ -9 10)) same (1))
                       (finished (1))))
  
  (test-upto-int/lam "(define (f2 x) (+ (g2 x) 10))"
                     `((finished ((define (f2 x) (+ (g2 x) 10))))))
  
  
  (test-upto-int/lam "(cons 1 2)" 
                     `((before-error (,h-p) ((cons 1 2)) "cons: second argument must be of type <list>, given 1 and 2")))
  
  (test-beginner-sequence "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
                          `((before-after-finished ((cons 3 (cons 1 empty)))
                                                   (,h-p) ((list 1 2 3)) same ((cons 1 (cons 2 (cons 3 empty)))))
                            (finished ((cons 1 (cons 2 (cons 3 empty))) (define-struct aa (b)) (make-aa 3)))))
  
  (test-bwla-to-int/lam "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
                        `((before-after ((cons 3 ,h-p)) ((cons 1 empty)) same ((list 1)))
                          (before-after (,h-p) ((cons 3 (list 1))) same ((list 3 1)))
                          (finished ((list 3 1) (list 1 2 3) (define-struct aa (b)) (make-aa 3)))))
  
  (test-upto-int/lam "(define a11 4)"
                     `((finished ((define a11 4)))))
  
  (test-mz-sequence "(map (lambda (x) x) (list 3 4 5))"
                    `((before-after ((map (lambda (x) x) ,h-p)) ((list 3 4 5))
                                    same (`( 3 4 5)))
                      (before-after (,h-p) ((map (lambda (x) x) `(3 4 5)))
                                    ((... ,h-p ...)) (3))
                      (before-after (,h-p) (...)
                                    ((... ,h-p ...)) (4))
                      (before-after (,h-p) (...)
                                    ((... ,h-p ...)) (5))
                      (before-after (,h-p) (...)
                                    (,h-p) (`(3 4 5)))
                      (finished (`(3 4 5)))))
  
  (test-beginner-wla-sequence "'(3 4 5)"
                              `((finished ((list 3 4 5)))))
  
  
  ;;;;;;;;;;;;;
  ;;
  ;;  QUASIQUOTE
  ;;
  ;;;;;;;;;;;;;.
  
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
  
  ;;;;;;;;;;;;;
  ;;
  ;;  LET
  ;;
  ;;;;;;;;;;;;;
  
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
  
  
;    (test-intermediate-sequence "(define a12 3) (define c12 19) (let ([a12 13] [b12 a12]) (+ b12 a12 c12))"
;                                `((before-after-finished ((define a12 3) (define c12 19))
;                                                         (,h-p) ((let ([a12 13] [b12 a12]) (+ b12 a12 c12))) 
;                                                         (,h-p ,h-p ,h-p) ((define a12_0 13) (define b12_1 a12) (+ b12_1 a12_0 c12)))
;                                  (before-after-finished ((define a12_0 13))
;                                                         ((define b12_1 ,h-p) (+ b12_1 a12_0 c)) (a12) same (3))
;                                  (before-after-finished ((define b12_1 3))
;                                                         ((+ ,h-p a12_0 c12)) (b12_1) same (3))
;                                  (before-after ((+ 3 ,h-p c12)) (a12_0) same (13))
;                                  (before-after ((+ 3 13 ,h-p)) (c12) same 19)
;                                  (before-after (,h-p) ((+ 3 13 19)) same (35))
;                                  (finished (35))))
;    
  ;  (test-intermediate-sequence "(let ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
  ;                              `((before-after (,h-p) ((let ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)) 
  ;                                              (,h-p ,h-p ,h-p) ((define a_0 (lambda (x) (+ x 14))) (define b_1 (+ 3 4)) 9))
  ;                                (before-after-finished ((define a_0 (lambda (x) (+ x 14))))
  ;                                                       ((define b_1 ,h-p) 9) ((+ 3 4)) same (7))
  ;                                (finished ((define b_1 7) 9))))
  ;  
  ;  (test-intermediate-sequence "(define (f g) (let ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) eps))]) gp)) (define gprime (f cos))"
  ;                              `((before-after-finished ((define (f g) (let ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) eps))]) gp)))
  ;                                                       ((define gprime ,h-p)) ((f cos))
  ;                                                       same ((let ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))]) gp)))
  ;                                (before-after ((define gprime ,h-p)) ((let ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))]) gp))
  ;                                              (,h-p (define gprime ,h-p)) ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))) gp_0))
  ;                                (finished ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))) (define gprime gp_0)))))
  ;  
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  LET*
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;  
  ;  (test-intermediate-sequence "(define a 3) (define c 19) (let* ([a 13] [b a]) (+ b a c))"
  ;                              `((before-after-finished ((define a 3) (define c 19))
  ;                                                       (,h-p) ((let* ([a 13] [b a]) (+ b a c))) 
  ;                                                       (,h-p ,h-p ,h-p) ((define a_0 13) (define b_1 a_0) (+ b_1 a_0 c)))
  ;                                (before-after-finished ((define a_0 13))
  ;                                                       ((define b_1 ,h-p) (+ b_1 a_0 c)) (a_0) same (13))
  ;                                (before-after-finished ((define b_1 13))
  ;                                                       ((+ ,h-p a_0 c)) (b_1) same (13))
  ;                                (before-after ((+ 13 ,h-p c)) (a_0) same (13))
  ;                                (before-after ((+ 13 13 ,h-p)) (c) same 19)
  ;                                (before-after (,h-p) ((+ 13 13 19)) same (45))
  ;                                (finished (45))))
  ;  
  ;  (test-intermediate-sequence "(let* ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
  ;                              `((before-after (,h-p) ((let* ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)) 
  ;                                              (,h-p ,h-p ,h-p) ((define a_0 (lambda (x) (+ x 14))) (define b_1 (+ 3 4)) 9))
  ;                                (before-after-finished ((define a_0 (lambda (x) (+ x 14))))
  ;                                                       ((define b_1 ,h-p) 9) ((+ 3 4)) same (7))
  ;                                (finished ((define b_1 7) 9))))
  ;  
  ;  (test-intermediate-sequence "(define (f g) (let* ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) eps))]) gp)) (define gprime (f cos))"
  ;                              `((before-after-finished ((define (f g) (let* ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) eps))]) gp)))
  ;                                                       ((define gprime ,h-p)) ((f cos))
  ;                                                       same ((let* ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))]) gp)))
  ;                                (before-after ((define gprime ,h-p)) ((let* ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))]) gp))
  ;                                              (,h-p (define gprime ,h-p)) ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))) gp_0))
  ;                                (finished ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))) (define gprime gp_0)))))
  ;  
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  LETREC
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;  
  ;  (test-intermediate-sequence "(define a 3) (define c 19) (letrec ([a 13] [b a]) (+ b a c))"
  ;                              `((before-after-finished ((define a 3) (define c 19))
  ;                                                       (,h-p) ((letrec ([a 13] [b a]) (+ b a c))) 
  ;                                                       (,h-p ,h-p ,h-p) ((define a_0 13) (define b_1 a_0) (+ b_1 a_0 c)))
  ;                                (before-after-finished ((define a_0 13))
  ;                                                       ((define b_1 ,h-p) (+ b_1 a_0 c)) (a_0) same (13))
  ;                                (before-after-finished ((define b_1 13))
  ;                                                       ((+ ,h-p a_0 c)) (b_1) same (13))
  ;                                (before-after ((+ 13 ,h-p c)) (a_0) same (13))
  ;                                (before-after ((+ 13 13 ,h-p)) (c) same 19)
  ;                                (before-after (,h-p) ((+ 13 13 19)) same (45))
  ;                                (finished (45))))
  ;  
  ;  (test-intermediate-sequence "(letrec ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
  ;                              `((before-after (,h-p) ((letrec ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)) 
  ;                                              (,h-p ,h-p ,h-p) ((define a_0 (lambda (x) (+ x 14))) (define b_1 (+ 3 4)) 9))
  ;                                (before-after-finished ((define a_0 (lambda (x) (+ x 14))))
  ;                                                       ((define b_1 ,h-p) 9) ((+ 3 4)) same (7))
  ;                                (finished ((define b_1 7) 9))))
  ;  
  ;  (test-intermediate-sequence "(define (f g) (letrec ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) eps))]) gp)) (define gprime (f cos))"
  ;                              `((before-after-finished ((define (f g) (letrec ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) eps))]) gp)))
  ;                                                       ((define gprime ,h-p)) ((f cos))
  ;                                                       same ((letrec ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))]) gp)))
  ;                                (before-after ((define gprime ,h-p)) ((letrec ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))]) gp))
  ;                                              (,h-p (define gprime ,h-p)) ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))) gp_0))
  ;                                (finished ((define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) eps))) (define gprime gp_0)))))
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  RECUR
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;  
  ;  (test-intermediate-sequence "(define (countdown n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))) (countdown 2)"
  ;                              `((before-after-finished ((define (countdown n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))))
  ;                                                       (,h-p) ((countdown 2)) same ((recur loop ([n 2]) (if (= n 0) 13 (loop (- n 1))))))
  ;                                (before-after (,h-p) ((recur loop ([n 2]) (if (= n 0) 13 (loop (- n 1))))) 
  ;                                              (,h-p ,h-p) ((define (loop_0 n) (if (= n 0) 13 (loop_0 (- n 1)))) ((loop_0 2))))
  ;                                (before-after-finished ((define (loop_0 n) (if (= n 0) 13 (loop_0 (- n 1)))))
  ;                                                       (,h-p) ((loop_0 2)) same ((if (= 2 0) 13 (loop_0 (- 2 1)))))
  ;                                (before-after ((if ,h-p 13 (loop_0 (- 2 1)))) ((= 2 0)) same (false))
  ;                                (before-after (,h-p) ((if false 13 (loop_0 (- 2 1)))) same ((loop_0 (- 2 1))))
  ;                                (before-after ((loop_0 ,h-p)) ((- 2 1)) same (1))
  ;                                (before-after (,h-p) ((loop_0 1)) same ((if (= 1 0) 13 (loop_0 (- 1 1)))))
  ;                                (before-after ((if ,h-p 13 (loop_0 (- 1 1)))) ((= 1 0)) same (false))
  ;                                (before-after (,h-p) ((if false 13 (loop_0 (- 1 1)))) same ((loop_0 (- 1 1))))
  ;                                (before-after ((loop_0 ,h-p)) ((- 1 1)) same (0))
  ;                                (before-after (,h-p) ((loop_0 0)) same ((if (= 0 0) 13 (loop_0 (- 0 1)))))
  ;                                (before-after ((if ,h-p 13 (loop_0 (- 0 1)))) ((= 0 0)) same (true))
  ;                                (before-after (,h-p) ((if true 13 (loop_0 (- 1 1)))) same (13))
  ;                                (finished (13))))
  ;
    ;;;;;;;;;;;;;
    ;;
    ;;  LOCAL
    ;;
    ;;;;;;;;;;;;;
    
  
    (test-intermediate-sequence "(local () (+ 3 4))"
                                `((before-after (,h-p) ((local () (+ 3 4)))
                                                (,h-p) ((+ 3 4)))
                                  (before-after (,h-p) ((+ 3 4))
                                                (,h-p) (7))
                                  (finished (7))))
    
    (test-intermediate-sequence "(local ((define (a x) (+ x 9))) (a 6))"
                                `((before-after (,h-p) ((local ((define (a x) (+ x 9))) (a 6)))
                                                (,h-p ,h-p) ((define (a_0 x) (+ x 9)) (a_0 6)))
                                  (before-after-finished ((define (a_0 x) (+ x 9)))
                                                         (,h-p) ((a_0 6)) same ((+ 6 9)))
                                  (before-after (,h-p) ((+ 6 9)) same (15))
                                  (finished (15))))
    
    (test-intermediate-sequence "(local ((define (a x) (+ x 13))) a)"
                                `((before-after (,h-p) ((local ((define (a x) (+ x 13))) a))
                                                (,h-p ,h-p) ((define (a_0 x) (+ x 13)) a_0))
                                  (finished ((define (a_0 x) (+ x 13)) a_0))))
  
    (test-intermediate-sequence "(local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))"
                                `((before-after (,h-p) ((local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1)))
                                                (,h-p ,h-p ,h-p ,h-p) ((define (a_0 x) (+ x 9)) (define b_1 a_0) (define p_2 (+ 3 4)) (b_1 1)))
                                  (before-after-finished-waiting ((define (a_0 x) (+ x 9)) (define b_1 a_0))
                                                                 ((define p_2 ,h-p)) ((+ 3 4)) same (7) ((b_1 1)))
                                  (before-after-finished ((define p_2 7))
                                                         ((,h-p 1)) (b_1) same (a_0))
                                  (before-after (,h-p) ((a_0 1)) same ((+ 1 9)))
                                  (before-after (,h-p) ((+ 1 9)) same (10))
                                  (finished (10))))
    
      (test-intermediate-sequence "(define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) eps)]) gp)) (define gprime (f12 cos))"
                                `((before-after-finished ((define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) eps)]) gp)))
                                                         ((define gprime ,h-p)) ((f12 cos))
                                                         same ((local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) eps)]) gp)))
                                  (before-after ((define gprime ,h-p)) ((local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) eps)]) gp))
                                                (,h-p (define gprime ,h-p)) ((define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) eps)) gp_0))
                                  (finished ((define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) eps)) (define gprime gp_0)))))
  
  ; test generativity... that is, multiple evaluations of a local get different lifted names:
  
  (test-intermediate-sequence "(define (a13 b13 c13) (b13 c13)) (define (f9 x) (local ((define (maker dc) x)) maker)) (define m1 (f9 3)) (a13 (f9 4) 1)"
                              `((before-after-finished ((define (a13 b13 c13) (b13 c13))
                                                        (define (f9 x) (local ((define (maker dc) x)) maker)))
                                                      ((define m1 ,h-p)) ((f9 3)) same ((local ((define (maker dc) 3)) maker)))
                                (before-after ((define m1 ,h-p)) ((local ((define (maker dc) 3)) maker))
                                              (,h-p (define m1 ,h-p)) ((define (maker_0 dc) 3) maker_0))
                                (before-after-finished ((define (maker_0 dc) 3) (define m1 maker_0))
                                                       ((a13 ,h-p 1)) ((f9 4)) same ((local ((define (maker dc) 4)) maker)))
                                (before-after ((a13 ,h-p 1)) ((local ((define (maker dc) 4)) maker))
                                              (,h-p (a13 ,h-p 1)) ((define (maker_1 dc) 4) maker_1))
                                (before-after-finished ((define (maker_1 dc) 4))
                                                       (,h-p) ((a13 maker_1 1)) same ((maker_1 1)))
                                (before-after (,h-p) ((maker_1 1)) same (4))
                                (finished (4))))
  
  
  ;  
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  TIME
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;  
  ;  (test-intermediate-sequence "(time (+ 3 4))"
  ;                              `((before-after ((time ,h-p)) ((+ 3 4)) same (7))
  ;                                (before-after (,h-p) ((time 7)) same (7))
  ;                                (result (7))))
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  TEACHPACK TESTS
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;  
  
  (require (lib "mred.ss" "mred"))
  
  (define tp-namespace
    (let ([ns (current-namespace)]
          [mred-name ((current-module-name-resolver) '(lib "mred.ss" "mred") #f #f)]
          [new-namespace (make-namespace 'empty)])
      (parameterize ([current-namespace new-namespace])
        (namespace-attach-module ns 'mzscheme)
        (namespace-attach-module ns mred-name)
        (namespace-require '(lib "htdp-beginner.ss" "lang"))
        (namespace-require '(lib "draw.ss" "htdp"))
        (namespace-require '(lib "servlet2.ss" "htdp"))
        new-namespace)))
  
  (define test-teachpack-sequence (lambda args
                                    (let ([new-custodian (make-custodian)])
                                      (parameterize ([current-custodian new-custodian])
                                        (apply (lang-level-test-sequence tp-namespace fake-beginner-render-settings) args))
                                      (custodian-shutdown-all new-custodian))))
  
    
    ; uses set-render-settings!
    ;(reconstruct:set-render-settings! fake-beginner-render-settings)
    ;(test-sequence "(define (check-guess guess target) 'TooSmall) (guess-with-gui check-guess)"
    ;               `((before-after (,h-p) ((guess-with-gui check-guess)))
    ;                 ((,h-p) (true)))
    ;               `((define (check-guess guess target) 'toosmall) true)
    ;               tp-namespace)
    
;  (test-teachpack-sequence 
; "(define (draw-limb i) (cond  
; [(= i 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)] 
; [(= i 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))  
; (and (start 100 100)
; (draw-limb 0))"
; `((before-after-finished ((define (draw-limb i) (cond [(= i 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)] 
;                                                       [(= i 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
;                          ((and ,h-p (draw-limb 0))) ((start 100 100))
;                          same (true))
;   (before-after (,h-p) ((and true (draw-limb 0)))
;                 same ((draw-limb 0)))
;   (before-after (,h-p) ((draw-limb 0))
;                 same ((cond [(= 0 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)] 
;                             [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
;   (before-after ((cond [,h-p (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)] 
;                        [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))
;                 ((= 0 1))
;                 same (false))
;   (before-after (,h-p) ((cond [false (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)] 
;                               [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))
;                 same ((cond [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
;   (before-after ((cond [,h-p (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])) ((= 0 0))
;                 same (true))
;   (before-after (,h-p) ((cond [true (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))
;                 same ((draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)))
;   (before-after ((draw-solid-line (make-posn ,h-p 10) (make-posn 10 100) 'red)) ((+ 1 10)) same (11))
;   (before-after (,h-p) ((draw-solid-line (make-posn 11 10) (make-posn 10 100) 'red)) same (true))
;   (finished (true))))
; 
; 
;  (test-teachpack-sequence
;"(define (adder go) (inform (number->string (+ (single-query (make-number \"enter 10\")) (single-query (make-number \"enter 20\"))))))
;(adder true)"
;`((before-after-finished ((define (adder go) (inform (number->string (+ (single-query (make-number "enter 10")) (single-query (make-number "enter 20")))))))
;                         (,h-p) ((adder true)) same ((inform (number->string (+ (single-query (make-number "enter 10")) (single-query (make-number "enter 20")))))))
;  (before-after ((inform (number->string (+ (single-query ,h-p) (single-query (make-number "enter 20")))))) ((make-number "enter 10")) 
;                same ((make-numeric "enter 10")))
;  (before-after ((inform (number->string (+ ,h-p (single-query (make-number "enter 20")))))) ((single-query (make-numeric "enter 10")))
;                same (10))
;  (before-after ((inform (number->string (+ 10 (single-query ,h-p))))) ((make-number "enter 20")) same ((make-numeric "enter 20")))
;  (before-after ((inform (number->string (+ 10 ,h-p)))) ((single-query (make-numeric "enter 20"))) same (20))
;  (before-after ((inform (number->string ,h-p))) ((+ 10 20)) same (30))
;  (before-after ((inform ,h-p)) ((number->string 30)) same "30")
;  (before-after (,h-p) ((inform "30")) same (true))
;  (finished (true))))
  )