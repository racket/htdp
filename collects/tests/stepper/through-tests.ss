(module through-test mzscheme
  (require (lib "private/shared.ss" "stepper")
           (lib "private/model.ss" "stepper")
           "tests-common.ss")
  
  
  (define (test-sequence namespace render-settings exp-str expected-steps)
    (let ([receive-result
           (lambda (result go-semaphore)
             (if (compare-steps result (car expected-steps))
                 (begin 
                   (set! expected-steps (cdr expected-steps))
                   (semaphore-post go-semaphore))
                 (error 'test-sequence "steps do not match.\ngiven: ~v\nexpected: ~v\n" result (car expected-steps))))]
          [expand-in-namespace
           (lambda (sexp)
             (parameterize ([current-namespace namespace])
               (expand sexp)))]
          [program-expander
           (lambda (init iter)
             (letrec ([input-port (make-string-input-port exp-str)]
                      [read-and-deliver
                       (lambda ()
                         (let ([new-exp (read input-port)])
                           (if (eof-object? new-exp)
                               (iter new-exp void)
                               (iter (expand-in-namespace new-exp) read-and-deliver))))])
               (init)
               (read-and-deliver)))])
      (go program-expander receive-result render-settings)))
  
  (define (lang-level-test-sequence ns rs)
    (lambda args
      (apply test-sequence ns rs args)))
  
  (define test-mz-sequence (lang-level-test-sequence mz-namespace fake-mz-render-settings))
  (define test-beginner-sequence (lang-level-test-sequence beginner-namespace fake-beginner-render-settings))
  (define test-beginner-wla-sequence (lang-level-test-sequence beginner-wla-namespace fake-beginner-wla-render-settings))
  (define test-intermediate-sequence (lang-level-test-sequence intermediate-namespace fake-intermediate-render-settings))
  (define test-intermediate-lambda-sequence (lang-level-test-sequence intermediate-lambda-namespace fake-intermediate-lambda-render-settings))
  
  
  (define (compare-steps expected actual)
    (case (car expected)
      [(before-after) (and )]
      
           ))
  
  
  
  
  ;;;;;;;;;;;;;
  ;;
  ;;  mz tests
  ;;
  ;;;;;;;;;;;;;
  
  (test-mz-sequence "(for-each (lambda (x) x) '(1 2 3))"
                    `(((,highlight-placeholder) ((for-each (lambda (x) x) `(1 2 3))))
                      (((... ,highlight-placeholder ...)) (1))
                      ((...) ())
                      (((... ,highlight-placeholder ...)) (2))
                      ((...) ())
                      (((... ,highlight-placeholder ...)) (3))
                      ((...) ())
                      ((,highlight-placeholder) ((void)))))
  
  (test-mz-sequence "(+ 3 4)"
                    `(((,highlight-placeholder) ((+ 3 4)))
                      ((,highlight-placeholder) (7))))
  
  (test-mz-sequence "((lambda (x) (+ x 3)) 4)"
                    `(((,highlight-placeholder) (((lambda (x) (+ x 3)) 4)))
                      ((,highlight-placeholder) ((+ 4 3)))
                      ((,highlight-placeholder) ((+ 4 3)))
                      ((,highlight-placeholder) (7))))
  
  (test-mz-sequence "(if 3 4 5)"
                    `(((,highlight-placeholder) ((if 3 4 5)))
                      ((,highlight-placeholder) (4))))
  
  (test-beginner-sequence "(if (if true false true) false true)"
                          `((((if ,highlight-placeholder false true)) ((if true false true)))
                            (((if ,highlight-placeholder false true)) (false))
                            ((,highlight-placeholder) ((if false false true)))
                            ((,highlight-placeholder) (true)))
                          `(true))
  
  (test-mz-sequence "((lambda (x) x) 3)"
                    `(((,highlight-placeholder) (((lambda (x) x) 3)))
                      ((,highlight-placeholder) (3))))
  
  ; 'begin' not yet supported by reconstruct
  ;(test-mz-sequence "((lambda (x) x) (begin (+ 3 4) (+ 4 5)"))
  ;                  `((((begin ,highlight-placeholder (+ 4 5))) ((+ 3 4)))
  ;                    (((begin ,highlight-placeholder (+ 4 5))) (7))
  ;                    ((,highlight-placeholder) ((begin 7 (+ 4 5))))
  ;                    ((,highlight-placeholder) ((+ 4 5)))
  ;                    ((,highlight-placeholder) ((+ 4 5)))
  ;                    ((,highlight-placeholder) (9))))
  
  (test-mz-sequence "((lambda (a) (lambda (b) (+ a b))) 14)"
                    `(((,highlight-placeholder) (((lambda (a) (lambda (b) (+ a b))) 14)))
                      ((,highlight-placeholder) ((lambda (b) (+ 14 b))))))
  
  (test-mz-sequence "((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)"
                    `(((,highlight-placeholder) (((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)))
                      ((,highlight-placeholder) ((+ 5 6)))
                      ((,highlight-placeholder) ((+ 5 6)))
                      ((,highlight-placeholder) (11))))
  
  ; reconstruct does not handle one-armed if's:
  ;(test-mz-sequence "(if 3 4)"
  ;                  `(((,highlight-placeholder) ((if 3 4)))
  ;                    ((,highlight-placeholder) (4))))
  
  ; reconstruct does not handle begin0
  
  ;(test-mz-sequence "(let ([a 3]) 4)"
  ;                  `(((,highlight-placeholder) ((let-values ([(a) 3]) 4)) (,highlight-placeholder ,highlight-placeholder) ((define-values (a_0) 3) (begin 4)))
  ;                    (((define a_0 3))))) 
  ;
  ;(test-mz-sequence "(let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))"
  ;                  `(((,highlight-placeholder) ((let-values ([(a) (+ 4 5)] [(b) (+ 9 20)]) (+ a b))) 
  ;                     (,highlight-placeholder ,highlight-placeholder ,highlight-placeholder) 
  ;                     ((define-values (a_0) (+ 4 5)) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))))
  ;                    (((define-values (a_0) ,highlight-placeholder) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) ((+ 4 5)))
  ;                    (((define-values (a_0) ,highlight-placeholder) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) (9))
  ;                    (((define a_0 9) (define-values (b_1) ,highlight-placeholder) (begin (+ a_0 b_1))) ((+ 9 20)))
  ;                    (((define a_0 9) (define-values (b_1) ,highlight-placeholder) (begin (+ a_0 b_1))) (29))
  ;                    (((define a_0 9) (define b_1 29)))
  ;                    (((+ ,highlight-placeholder b_1)) (a_0))
  ;                    (((+ ,highlight-placeholder b_1)) (9))
  ;                    (((+ 9 ,highlight-placeholder)) (b_1))
  ;                    (((+ 9 ,highlight-placeholder)) (29))
  ;                    ((,highlight-placeholder) ((+ 9 29)))
  ;                    ((,highlight-placeholder) (38))))
  
  ;(test-mz-sequence "((call-with-current-continuation call-with-current-continuation) (call-with-current-continuation call-with-current-continuation))"
  ;                  `((((,highlight-placeholder (call-with-current-continuation call-with-current-continuation))) ((call-with-current-continuation call-with-current-continuation)))
  ;                    (((,highlight-placeholder (call-with-current-continuation call-with-current-continuation))) ((lambda args ...)))
  ;                    ((((lambda args ...) ,highlight-placeholder)) ((call-with-current-continuation call-with-current-continuation)))
  ;                    ((((lambda args ...) ,highlight-placeholder)) ((lambda args ...)))))
  
  ;(test-mz-sequence '(begin (define g 3) g)
  ;                  `(((,highlight-placeholder) (g))
  ;                    ((,highlight-placeholder) 3)))
  
  ;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))
  
  (test-beginner-sequence "(define a (+ 3 4))"
                          `((((define a ,highlight-placeholder)) ((+ 3 4)))
                            (((define a ,highlight-placeholder)) (7)))
                          `((define a 7)))
  
  (test-beginner-sequence "(+ 4 129)"
                          `(((,highlight-placeholder) ((+ 4 129)))
                            ((,highlight-placeholder) (133)))
                          `(133))
  
  (test-beginner-sequence "(if true 3 4)"
                          `(((,highlight-placeholder) ((if true 3 4)))
                            ((,highlight-placeholder) (3)))
                          `(3))
  
  ;;;;;;;;;;;;;
  ;;
  ;;  COND
  ;;
  ;;;;;;;;;;;;;
  
  (parameterize ([current-namespace beginner-namespace])
    (let* ([stx (expand (car (string->stx-list "(cond [else 3])")))]
           [stx-source (syntax-source stx)]
           [stx-posn (syntax-position stx)])
      (printf "expanded: ~a\n" (syntax-object->datum stx))
      (syntax-case stx (if begin #%datum)
        [(if dc dc2 stx2)
         (printf "stepper-else: ~a\n" (syntax-property stx 'stepper-else))
         ]
        [stx
         (printf "outer thing has wrong shape: ~a\n" (syntax-object->datum (syntax stx)))])))
  
  (test-beginner-sequence "(cond [false 4] [false 5] [true 3])"
                          `(((,highlight-placeholder) ((cond (false 4) (false 5) (true 3))))
                            ((,highlight-placeholder) ((cond (false 5) (true 3))))
                            ((,highlight-placeholder) ((cond (false 5) (true 3))))
                            ((,highlight-placeholder) ((cond (true 3))))
                            ((,highlight-placeholder) ((cond (true 3))))
                            ((,highlight-placeholder) (3)))
                          `(3))
  
  (test-beginner-sequence "(cond [false 4] [else 9])"
                          `(((,highlight-placeholder) ((cond [false 4] [else 9])))
                            ((,highlight-placeholder) ((cond [else 9])))
                            ((,highlight-placeholder) ((cond [else 9])))
                            ((,highlight-placeholder) (9)))
                          `(9))
  
  (test-beginner-sequence "(cond [true 3] [else (and true true)])"
                          `(((,highlight-placeholder) ((cond (true 3) (else (and true true)))))
                            ((,highlight-placeholder) (3)))
                          `(3))
  
  
  ; syntactic error: (test-beginner-sequence "(cond)")
  
  (test-beginner-sequence "(cond [else 3])"
                          `(((,highlight-placeholder) ((cond (else 3))))
                            ((,highlight-placeholder) (3)))
                          `(3))
  
  (test-beginner-sequence "(cond [else (cond [else 3])])"
                          `(((,highlight-placeholder) ((cond (else (cond (else 3))))))
                            ((,highlight-placeholder) ((cond (else 3))))
                            ((,highlight-placeholder) ((cond (else 3))))
                            ((,highlight-placeholder) (3)))
                          `(3))
  
  ; reconstruct can't handle begin
  ;(test-mz-sequence "(cond [#f 3 4] [#t (+ 3 4) (+ 4 9)])"
  ;                  `(((,highlight-placeholder) ((cond (#f 3 4) (#t (+ 3 4) (+ 4 9)))))
  ;                    ((,highlight-placeholder) ((cond (#t (+ 3 4) (+ 4 9)))))
  ;                    ((,highlight-placeholder) ((cond (#t (+ 3 4) (+ 4 9)))))
  ;                    ((,highlight-placeholder) (begin (+ 3 4) (+ 4 9)))
  ;                    (((begin ,highlight-placeholder (+ 4 9))) ((+ 3 4)))
  ;                    (((begin ,highlight-placeholder (+ 4 9)))  (7))
  ;                    ((,highlight-placeholder) ((begin 7 (+ 4 9))))
  ;                    ((,highlight-placeholder) ((+ 4 9)))
  ;                    ((,highlight-placeholder) ((+ 4 9)))
  ;                    ((,highlight-placeholder) (13))))
  ;
  
  
  (test-beginner-sequence "(cond [false 3] [else (cond [true 4])])"
                          `(((,highlight-placeholder) ((cond (false 3) (else (cond (true 4))))))
                            ((,highlight-placeholder) ((cond (else (cond (true 4))))))
                            ((,highlight-placeholder) ((cond (else (cond (true 4))))))
                            ((,highlight-placeholder) ((cond (true 4))))
                            ((,highlight-placeholder) ((cond (true 4))))
                            ((,highlight-placeholder) (4)))
                          `(4))
  
  ;;;;;;;;;;;;;
  ;;
  ;;  OR / AND
  ;;
  ;;;;;;;;;;;;;
  
  (test-beginner-sequence "(or false true false)"
                          `(((,highlight-placeholder) ((or false true false)))
                            ((,highlight-placeholder) ((or true false)))
                            ((,highlight-placeholder) ((or true false)))
                            ((,highlight-placeholder) (true)))
                          `(true))
  
  (test-beginner-sequence "(and true false true)"
                          `(((,highlight-placeholder) ((and true false true)))
                            ((,highlight-placeholder) ((and false true)))
                            ((,highlight-placeholder) ((and false true)))
                            ((,highlight-placeholder) (false)))
                          `(false))
  
  (parameterize ([current-namespace beginner-namespace])
    (map syntax-object->datum
         ;(map expand
         (annotate-exprs (map expand (list '(define (a19 x) x) '(a19 4))) (lambda (x y z) 3))
         ;)
         ))
  
  (parameterize ([current-namespace beginner-namespace])
    (map syntax-object->datum
         (map expand (map expand (map expand (list 'a19))))))
  
  (test-beginner-sequence "(define (a2 x) x) (a2 4)"
                          `(((,highlight-placeholder) ((a2 4)))
                            ((,highlight-placeholder) (4)))
                          `((define (a2 x) x) 4))
  
  (test-beginner-sequence "(define (a3 x) (if true x x)) (a3 false)"
                          `(((,highlight-placeholder) ((a3 false)))
                            ((,highlight-placeholder) ((if true false false)))
                            ((,highlight-placeholder) ((if true false false)))
                            ((,highlight-placeholder) (false)))
                          `((define (a3 x) (if true x x)) false))
  
  (test-beginner-sequence "(define (b2 x) (and true x)) (b2 false)"
                          `(((,highlight-placeholder) ((b2 false)))
                            ((,highlight-placeholder) ((and true false)))
                            ((,highlight-placeholder) ((and true false)))
                            ((,highlight-placeholder) (false)))
                          `((define (b2 x) (and true x)) false))
  
  (test-beginner-sequence "(define a1 true)(define (b1 x) (and a1 true x)) (b1 false)"
                          `(((,highlight-placeholder) ((b1 false)))
                            ((,highlight-placeholder) ((and a1 true false)))
                            (((and ,highlight-placeholder true false)) (a1))
                            (((and ,highlight-placeholder true false)) (true))
                            ((,highlight-placeholder) ((and true true false)))
                            ((,highlight-placeholder) ((and true false)))
                            ((,highlight-placeholder) ((and true false)))
                            ((,highlight-placeholder) (false)))
                          `((define a1 true) (define (b1 x) (and a1 true x)) false))
  
  
  (test-intermediate-sequence "(define a4 +) a4"
                              `(((,highlight-placeholder) (a4))
                                ((,highlight-placeholder) (+)))
                              `((define a4 +) +))
  
  (test-intermediate-sequence "(define (f123 x) (+ x 13)) f123"
                              `()
                              `((define (f123 x) (+ x 13)) f123))
  
  (test-beginner-sequence "(define (b x) (+ x 13)) (b 9)"
                          `(((,highlight-placeholder) ((b 9)))
                            ((,highlight-placeholder) ((+ 9 13)))
                            ((,highlight-placeholder) ((+ 9 13)))
                            ((,highlight-placeholder) (22)))
                          `((define (b x) (+ x 13)) 22))
  
  (test-beginner-sequence "(define-struct mamba (rhythm tempo)) (mamba-rhythm (make-mamba 24 2))"
                          `(((,highlight-placeholder) ((mamba-rhythm (make-mamba 24 2))))
                            ((,highlight-placeholder) (24)))
                          `((define-struct mamba (rhythm tempo)) 24))
  
  (test-beginner-sequence "(define a5 (lambda (a5) (+ a5 13))) (a5 23)"
                          `(((,highlight-placeholder) ((a5 23)))
                            ((,highlight-placeholder) ((+ 23 13)))
                            ((,highlight-placeholder) ((+ 23 13)))
                            ((,highlight-placeholder) (36)))
                          `((define a5 (lambda (a5) (+ a5 13))) 36))
  
  (test-beginner-sequence "(define c1 false) (define (d2 x) (or c1 false x)) (d2 false)"
                          `(((,highlight-placeholder) ((d2 false)))
                            ((,highlight-placeholder) ((or c1 false false)))
                            (((or ,highlight-placeholder false false)) (c1))
                            (((or ,highlight-placeholder false false)) (false))
                            ((,highlight-placeholder) ((or false false false)))
                            ((,highlight-placeholder) ((or false false)))
                            ((,highlight-placeholder) ((or false false)))
                            ((,highlight-placeholder) (false)))
                          `((define c1 false) (define (d2 x) (or c1 false x)) false))
  
  (test-beginner-sequence "(define (silly-choice str)
                             (string-append str (if false str str) str))
  (silly-choice \"family\")"
                          `(((,highlight-placeholder) ((silly-choice "family")))
                            ((,highlight-placeholder) ((string-append "family" (if false "family" "family") "family")))
                            (((string-append "family" ,highlight-placeholder "family")) ((if false "family" "family")))
                            (((string-append "family" ,highlight-placeholder "family")) ("family"))
                            ((,highlight-placeholder) ((string-append "family" "family" "family")))
                            ((,highlight-placeholder) ("familyfamilyfamily")))
                          '((define (silly-choice str) (string-append str (if false str str) str)) "familyfamilyfamily"))
  
  (test-beginner-sequence "(define (f x) (+ (g x) 10)) (define (g x) (- x 22)) (f 13)"
                          `(((,highlight-placeholder) ((f 13)))
                            ((,highlight-placeholder) ((+ (g 13) 10)))
                            (((+ ,highlight-placeholder 10)) ((g 13)))
                            (((+ ,highlight-placeholder 10)) ((- 13 22)))
                            (((+ ,highlight-placeholder 10)) ((- 13 22)))
                            (((+ ,highlight-placeholder 10)) (-9))
                            ((,highlight-placeholder) ((+ -9 10)))
                            ((,highlight-placeholder) (1)))
                          `((define (f x) (+ (g x) 10)) (define (g x) (- x 22)) 1))
  
  (test-beginner-sequence "(define (f2 x) (+ (g2 x) 10))"
                          `()
                          `((define (f2 x) (+ (g2 x) 10))))
  
  (err/rt-test (test-beginner-sequence "(cons 1 2)" `() `()) exn:application:type?)
  
  (test-beginner-sequence "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
                          `(((,highlight-placeholder) ((list 1 2 3)))
                            ((,highlight-placeholder) ((cons 1 (cons 2 (cons 3 empty))))))
                          `((cons 3 (cons 1 empty)) (cons 1 (cons 2 (cons 3 empty))) (define-struct aa (b)) (make-aa 3)))
  
  (test-beginner-sequence "(define a11 4)"
                          `()
                          `((define a11 4)))
  
  (test-mz-sequence "(map (lambda (x) x) (list 3 4 5))"
                    `((((map (lambda (x) x) ,highlight-placeholder)) ((list 3 4 5)))
                      (((map (lambda (x) x) ,highlight-placeholder)) (`( 3 4 5)))
                      ((,highlight-placeholder) ((map (lambda (x) x) `(3 4 5))))
                      (((... ,highlight-placeholder ...)) (3))
                      ((...) ())
                      (((... ,highlight-placeholder ...)) (4))
                      ((...) ())
                      (((... ,highlight-placeholder ...)) (5))
                      ((...) ())
                      ((,highlight-placeholder) (`(3 4 5)))))
  
  (test-beginner-wla-sequence "'(3 4 5)"
                              `()
                              `((list 3 4 5)))
  
  ; note: we currently punt on trying to unwind quasiquote.
  
  (test-beginner-wla-sequence "`(3 4 ,(+ 4 5))"
                              `((((cons 3 (cons 4 (cons ,highlight-placeholder empty)))) ((+ 4 5)))
                                (((cons 3 (cons 4 (cons ,highlight-placeholder empty)))) (9))
                                (((cons 3 (cons 4 ,highlight-placeholder))) ((cons 9 empty)))
                                (((cons 3 (cons 4 ,highlight-placeholder))) ((list 9)))
                                (((cons 3 ,highlight-placeholder)) ((cons 4 (list 9))))
                                (((cons 3 ,highlight-placeholder)) ((list 4 9)))
                                ((,highlight-placeholder) ((cons 3 (list 4 9))))
                                ((,highlight-placeholder) ((list 3 4 9))))
                              `((list 3 4 9)))
  
  (test-beginner-wla-sequence "`(3 ,@(list (+ 3 4) 5) 6)"
                              `((((cons 3 (append (list ,highlight-placeholder 5) (cons 6 empty)))) ((+ 3 4)))
                                (((cons 3 (append (list ,highlight-placeholder 5) (cons 6 empty)))) (7))
                                (((cons 3 (append (list 7 5) ,highlight-placeholder))) ((cons 6 empty)))
                                (((cons 3 (append (list 7 5) ,highlight-placeholder))) ((list 6)))
                                (((cons 3 ,highlight-placeholder)) ((append (list 7 5) (list 6))))
                                (((cons 3 ,highlight-placeholder)) ((list 7 5 6)))
                                ((,highlight-placeholder) ((cons 3 (list 7 5 6))))
                                ((,highlight-placeholder) ((list 3 7 5 6))))
                              `((list 3 7 5 6)))
  
  (test-intermediate-sequence "(local () (+ 3 4))"
                              `(((,highlight-placeholder) ((local () (+ 3 4))))
                                ((,highlight-placeholder) ((+ 3 4)))
                                ((,highlight-placeholder) ((+ 3 4)))
                                ((,highlight-placeholder) (7)))
                              `(7))
  
  (test-intermediate-sequence "(local ((define (a x) (+ x 9))) (a 6))"
                              `((())))
  
  (test-intermediate-sequence "(local ((define (a x) (+ x 13))) a)"
                              `((())))
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
  
  (reconstruct:set-render-settings! fake-beginner-render-settings)
  (test-sequence "(define (check-guess guess target) 'TooSmall) (guess-with-gui check-guess)"
                 `(((,highlight-placeholder) ((guess-with-gui check-guess)))
                   ((,highlight-placeholder) (true)))
                 `((define (check-guess guess target) 'toosmall) true)
                 tp-namespace)
  
  
  
  (report-errs)
  
  
  )