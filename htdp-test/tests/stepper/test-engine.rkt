#lang racket/base

(require syntax/modread
         stepper/private/shared
         stepper/private/shared-typed
         stepper/private/syntax-hider
         stepper/private/model
         tests/utils/sexp-diff
         lang/run-teaching-program
         racket/string
         racket/match
         racket/contract
         racket/file
         mzlib/pconvert-prop ;; so it can be attached.
         racket/gui/base ;; stepper big-bang tests need them attached
         test-engine/test-markup
         lang/private/rewrite-error-message
         test-engine/test-engine
         "language-level-model.rkt")

;; framework for simulating DrS for running stepper test cases.
;; this file currently supports two different testing setups; one
;; is the old style, pre #lang (before 2015), when student-level
;; languages did not expand into a single module. The second
;; is for programs starting with (e.g.) #lang htdp/bsl. Presumably,
;; the first kind will be deleted when we switch to all-#lang.


(provide
 (contract-out
  [string->expanded-syntax-list
   (-> ll-model? string? (listof syntax?))]
  [run-one-test (-> symbol? stepper-test? boolean?)]
  [struct stepper-test ([models (listof ll-model?)]
                        [string string?]
                        [expected-steps (listof step?)]
                        [extra-files (listof (list/c string? string?))])]
  [prepare-filesystem (-> string? (listof string?)
                          (list/c input-port? string? (-> void?)))]
  [create-provider-thunk
   (-> any/c boolean? input-port?
       (-> (-> (or/c syntax? eof-object?))))]
  [add-hashlang-line (-> string? string? string?)]
  [create-hashlang-provider-thunk
   (-> string? boolean? input-port?
       (-> (-> (or/c syntax? eof-object?))))]
  [filename->spec (-> string? symbol?)]

  ))

;; model-or-models/c string? (listof step?) (listof (list/c string? string?))

(struct stepper-test (models string expected-steps extra-files))

;; A SIMPLE EXAMPLE OF USING THIS FRAMEWORK:

;; note that this example uses the abbreviation from test-abbrev; don't uncomment it!

#;
(let* ([defs1 `((define (a x) (+ x 5)) (define b a))]
       [defs2 (append defs1 `((define c a)))])
  (apply         ;; you can abstract over this application with a define-syntax
   run-one-test  
   (tt 'top-ref4                   ;; - the name of the test
       m:intermediate              ;; - the language level (or levels) to run in
       ,@defs1 (define c b) (c 3)  ;; - the expressions to test (everything up to the first ::)
       :: ,@defs1 (define c {b})   ;; - the steps; the '::' divides steps, repeated '->'s indicate
       -> ,@defs1 (define c {a})   ;;    that the 'before' of the second step is the 'after' of
       :: ,@defs2 ({c} 3)          ;;    the first one.  the curly braces indicate the hilighted sexp.
       -> ,@defs2 ({a} 3)
       :: ,@defs2 {(a 3)}
       -> ,@defs2 {(+ 3 5)}
       -> ,@defs2 {8})))




;; PARAMETERS THAT CONTROL TESTING

(provide test-directory
         display-only-errors
         show-all-steps
         disable-stepper-error-handling
         ignore-non-lang-tests?)

(define test-directory (find-system-path 'temp-dir))

;; use this parameter to suppress output except in error cases:
(define display-only-errors (make-parameter #f))

;; use this parameter to show successful steps as well as unsuccessful ones:
(define show-all-steps (make-parameter #f))

;; use this parameter to prevent the stepper from capturing errors
;; (so that you can take advantage of DrRacket's error reporting)
(define disable-stepper-error-handling (make-parameter #f))

;; if a test takes more than this many seconds, it's a failure:
(define MAX-TEST-WAIT 2)

;; this parameter causes all tests of old non-#lang-style programs
;; to trivially succeed
(define ignore-non-lang-tests? (make-parameter #f))

;; DATA DEFINITIONS:

;; a step is one of 
;; - `(before-after ,before ,after) where before and after are sexp-with-hilite's
;; - `(error ,err-msg) where err-msg is a string
;; - `(before-error ,before ,err-msg) where before is an sexp-with-hilite and err-msg is a string
;; - `(finished-stepping)
;; or
;; - `(repetition ,(>=/c 0) ,(>/c 0) ,(non-empty-listof step)) (possible skippable) repetitions
;; or
;; - `(ignore)
(define (step? sexp)
  (match sexp
    [`(repetition ,lo ,hi ,rep-expected-steps)
     (and (exact-integer? lo) (>= lo 0)
          (exact-integer? hi) (> hi 0)
          (not (null? rep-expected-steps))
          (andmap step? rep-expected-steps))]
    [(list 'before-after before after) #t]
    [(list 'error (? string? msg)) #t]
    [(list 'before-error before (? string? msg)) #t]
    [(list 'finished-stepping) #t]
    [(list 'ignore) #t]
    [else #f]))

;; THE METHOD THAT RUNS A TEST:


;; run-one-test : symbol? stepper-test? -> boolean?

;; the ll-model determines the behavior of the stepper w.r.t. "language-level"-y things:
;; how should values be rendered, should steps be displayed (i.e, will the input & output
;; steps look just the same), etc. If 

;; the string contains a program to be evaluated. The string is an ironclad if blunt way
;; of ensuring that the program has no syntax information associated with it.

;; the steps lists the desired steps.  The easiest way to understand these is probably just to 
;; read the code for the comparison given in "compare-steps", below.

;; the extra-files contain a list of other files that must occur in the same directory. 

;; run the named test, return #t if a failure occurred during the test.

(define (run-one-test name the-test)
  (match the-test
    [(struct stepper-test (models exp-str expected-steps extra-files))
     (unless (display-only-errors)
       (printf "running test: ~v\n" name))
     (let ([error-has-occurred-box (box #f)])
       (for ([model (in-list models)])
         (test-sequence model exp-str expected-steps
                               extra-files error-has-occurred-box))
       (if (unbox error-has-occurred-box)
           (begin (eprintf "...Error has occurred during test: ~v\n" name)
                  #f)
           #t))]))

;; given a model and a string, return the fully expanded source.
(define (string->expanded-syntax-list ll-model exp-str)
  (cond
    [(ll-ll-model? ll-model)
     (run-with-testing-namespace
      (λ ()
        (match-define (list input-port filename done-thunk)
          (prepare-filesystem exp-str null))
        (define expander-thunk-thunk
          (create-provider-thunk (ll-ll-model-namespace-spec ll-model)
                                 (ll-ll-model-enable-testing? ll-model)
                                 input-port))
        (define expander-thunk (expander-thunk-thunk))
        (let loop ()
          (define next (expander-thunk))
          (cond [(eof-object? next) (begin (done-thunk) '())]
                [else (cons next (loop))]))))]
    [(ll-hashlang-model? ll-model)
     (parameterize ([current-directory test-directory])
       (run-with-testing-namespace
        (λ ()
          (match-define (list input-port filename done-thunk)
            (prepare-filesystem (add-hashlang-line
                                 (ll-hashlang-model-name ll-model)
                                 exp-str)
                                null))
          (define expander-thunk-thunk
            (create-hashlang-provider-thunk
             filename
             (ll-hashlang-model-enable-testing? ll-model)
             input-port))
          (define expander-thunk (expander-thunk-thunk))
          (let loop ()
            (define next (expander-thunk))
            (cond [(eof-object? next) (begin (done-thunk) '())]
                  [else (cons next (loop))])))))]))


;; run the thunk with a fresh base namespace with testing modules attached
(define (run-with-testing-namespace thunk)
  (define orig-namespace (current-namespace))
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-attach-module orig-namespace 'mzlib/pconvert-prop)
    (namespace-attach-module orig-namespace 'racket/class)
    (namespace-attach-module orig-namespace 'racket/gui/base)
    (namespace-attach-module orig-namespace 'test-engine/racket-tests)
    (thunk)))

;; test-sequence : ll-model? string? steps? extra-files? -> (void)
;; given a language model and an expression and a sequence of steps,
;; check to see whether the stepper produces the desired steps
(define (test-sequence the-ll-model exp-str expected-steps
                       extra-files error-box)
  (parameterize ([current-directory test-directory])
    (unless (display-only-errors)
      (printf "testing string: ~v\n" exp-str))
    (run-with-testing-namespace
     (λ ()
       (match the-ll-model
         [(struct ll-ll-model (namespace-spec render-settings enable-testing?))
          (cond [(not (ignore-non-lang-tests?))
                 (unless (display-only-errors)
                   (printf "  using language level ~v\n" namespace-spec))
                 (namespace-require 'test-engine/racket-tests)
                 (initialize-test-object!)
                 (match-define (list input-port filename done-thunk)
                   (prepare-filesystem exp-str extra-files))
                 (define provider-thunk
                   (create-provider-thunk namespace-spec enable-testing? input-port))
                 (define (dynamic-requirer) (void))
                 (test-sequence/core render-settings provider-thunk dynamic-requirer
                                     expected-steps error-box)
                 (done-thunk)]
                [else
                 (unless (display-only-errors)
                   (printf "ignoring non-lang test of string: ~v\n" exp-str))])]
         [(struct ll-hashlang-model (name render-settings enable-testing?))
          (unless (display-only-errors)
            (printf "  using language level ~v\n" name))
          (initialize-test-object!)
          (match-define (list input-port filename done-thunk)
            (prepare-filesystem (add-hashlang-line
                                 name
                                 exp-str)
                                extra-files))
          (define provider-thunk
            (create-hashlang-provider-thunk filename enable-testing? input-port))
          (define (dynamic-requirer)
            (define modspec (list 'quote (filename->spec filename)))
            (dynamic-require modspec #f)
            (namespace-require modspec)
            (define submod-spec `(submod ,modspec test))
            (when (module-declared? submod-spec)
              (dynamic-require submod-spec #f)))
          (test-sequence/core render-settings provider-thunk dynamic-requirer
                              expected-steps error-box)
          (done-thunk)])))))

;; add the #lang line to a string
(define (add-hashlang-line lang-str str)
  (string-append "#lang "lang-str"\n\n"
                 str))

;; given a language level model and string representing a program and a list of
;; extra files, return a port opened on a file containing the program,
;; along with a thunk to be called when finished.
(define (prepare-filesystem exp-str extra-files)
  (for ([f (in-list extra-files)])
    (display-to-file (cadr f)
                     (build-path test-directory (car f))
                     #:exists 'truncate))
  (define temp-file (make-temporary-file "stepper-temp-~a.rkt" #f
                                         test-directory))
  (define-values (_1 temp-file-name _2) (split-path temp-file))
  (display-to-file exp-str
                   temp-file
                   #:exists 'truncate)
  (define input-port (open-input-file temp-file))
  (port-count-lines! input-port)
  (list input-port
        (path->string temp-file-name)
        (lambda ()
          (close-input-port input-port)
          (delete-file temp-file)
          (for ([f (in-list extra-files)])
            (delete-file (build-path test-directory (car f)))))))

;; extract the stem of a path for use in a dynamic-require,
;; e.g. "foo.rkt" -> 'foo
(define (filename->spec filename)
  (match filename
    [(regexp #px"^([^/]*)\\.rkt$" (list _ stem))
     (string->symbol stem)]
    [other
     (raise-argument-error 'filename->spec
                           "name of racket file"
                           0 filename)]))

;; given the namespace-spec, whether testing is enabled, and an input-port,
;; return a thunk that when evaluated returns a thunk that produces
;; expanded expressions from the file, one at a time.
;; for use with language-level situations.
(define (create-provider-thunk namespace-spec enable-testing? input-port)
  ;; thunk this so that syntax errors happen within the error handlers:
  (lambda ()
    (let ([module-id (gensym "stepper-module-name-")])
      (expand-teaching-program input-port
                               (λ (name port)
                                 (parameterize ([read-decimal-as-inexact #f])
                                   (read-syntax name port)))
                               namespace-spec '()
                               module-id enable-testing?))))

;; can we at least re-use this?
(define expansion-namespace (make-base-namespace))

;; given the filename, whether testing is enabled, and an input port,
;; return a thunk that when evaluated returns a thunk that produces
;; expanded expressions from the file, one at a time.
;; for use with #lang situations.

(define (create-hashlang-provider-thunk filename enable-testing? input-port)
  ;; thunk this so that syntax errors happen within the error handlers:
  (lambda ()
    (list-then-eof
     (parameterize (;;could this be causing the problem? Hmm, no...
                    #;[current-namespace expansion-namespace])
       (list
        (expand
         (with-module-reading-parameterization
             (lambda ()
               (read-syntax (string->path filename) input-port)))))))))

;; produce a thunk that returns elements from a list, then ever after
;; returns #<eof>
(define (list-then-eof l)
  (let ([remaining (box l)])
    (lambda ()
      (cond [(null? (unbox remaining)) eof]
            [else (define next (car (unbox remaining)))
                  (set-box! remaining (cdr (unbox remaining)))
                  next]))))

(struct sstk-data ([warns #:mutable] [hd #:mutable])
  #:transparent)
(struct sstk-last sstk-data ()
  #:transparent)
(struct sstk-cons sstk-data ([received-results #:mutable] rep-hi rep-steps tl)
  #:transparent)

(define (steps-stack-last? stk)
  (sstk-last? stk))

(define (make-steps-stack all-steps)
  (sstk-last '() all-steps))

(define (steps-stack-push stk repetition)
  (match-define `(repetition 0 ,hi ,rep-expected-steps)
    repetition)
  (sstk-cons '() rep-expected-steps '() hi rep-expected-steps stk))

(define (steps-stack-pop stk)                  (sstk-cons-tl stk))
(define (steps-stack-top-repetition-hi stk)    (sstk-cons-rep-hi stk))
(define (steps-stack-top-repetition-steps stk) (sstk-cons-rep-steps stk))
(define (steps-stack-top stk)                  (sstk-data-hd stk))

(define (steps-stack-top-next! stk)
  (set-sstk-data-hd! stk (cdr (sstk-data-hd stk))))
(define (steps-stack-top-prepend! stk new-steps)
  (set-sstk-data-hd! stk (append new-steps (sstk-data-hd stk))))

(define (steps-stack-top-received-results stk)
  (reverse (sstk-cons-received-results stk)))

(define (steps-stack-top-receive! stk result)
  (when (sstk-cons? stk)
    (set-sstk-cons-received-results! stk
                                     (cons result (sstk-cons-received-results stk)))))

(define (steps-stack-top-warn stk error-box name fmt . args)
  (set-sstk-data-warns! stk
                        (cons (list error-box name fmt args)
                              (sstk-data-warns stk))))

(define (steps-stack-top-commit-warns! stk)
  (for ([warn-data (in-list (sstk-data-warns stk))])
    (match-define (list error-box name fmt args)
      warn-data)
    (apply warn error-box name fmt args))
  (set-sstk-data-warns! stk '()))

;; this is a front end for calling the stepper's "go"; the main 
;; responsibility here is to fake the behavior of DrRacket and collect the
;; resulting steps.
(define (test-sequence/core render-settings expanded-thunk dynamic-requirer expected-steps error-box)
  (define current-error-display-handler (error-display-handler))
  ;; stacks of all-steps for backtracking
  (define all-steps-stk (make-steps-stack expected-steps))
  ;; the values of certain parameters aren't surviving; create
  ;; lexical bindings for them:
  (define current-show-all-steps (show-all-steps))
  (define current-display-only-errors (display-only-errors))
  (define test-finished-semaphore (make-semaphore))
  (define receive-result
    (λ (result)
      (if (null? (steps-stack-top all-steps-stk))
          (begin (warn error-box
                       'test-sequence
                       "ran out of expected steps. Given result: ~v" result)
                 ;; test is finished, release the semaphore:
                 (semaphore-post test-finished-semaphore))
          (match (car (steps-stack-top all-steps-stk))
            ;; non-skippable repetition => unroll it
            [`(repetition ,lo ,hi ,rep-expected-steps)
             #:when (> lo 0)
             (steps-stack-top-next! all-steps-stk)
             (define unrolled-expected-steps
               (append rep-expected-steps
                       (if (> hi 0)
                           `((repetition ,(sub1 lo) ,(sub1 hi) ,rep-expected-steps))
                           '())))
             (steps-stack-top-prepend! all-steps-stk unrolled-expected-steps)
             (receive-result result)]
            [`(repetition 0 ,hi ,rep-expected-steps)
             ;; lo = 0, the repetition form can match ε
             (steps-stack-top-next! all-steps-stk)
             (set! all-steps-stk
                   (steps-stack-push all-steps-stk `(repetition 0 ,hi ,rep-expected-steps)))
             (receive-result result)]
            [expected
             (steps-stack-top-receive! all-steps-stk result)
             (cond
               [(compare-steps result expected error-box all-steps-stk)
                (when (and current-show-all-steps (not current-display-only-errors))
                  (printf "test-sequence: steps match for expected result: ~v\n"
                          expected))
                (steps-stack-top-next! all-steps-stk)
                (when (null? (steps-stack-top all-steps-stk))
                  (cond [(steps-stack-last? all-steps-stk)
                         (steps-stack-top-commit-warns! all-steps-stk)
                         ;; test is happy, release the semaphore:
                         (semaphore-post test-finished-semaphore)]
                        [(> (steps-stack-top-repetition-hi all-steps-stk) 1)
                         (define rep-step
                           `(repetition
                             0
                             ,(- (steps-stack-top-repetition-hi all-steps-stk) 1)
                             ,(steps-stack-top-repetition-steps all-steps-stk)))
                         (steps-stack-top-commit-warns! all-steps-stk)
                         (set! all-steps-stk (steps-stack-pop all-steps-stk))
                         (steps-stack-top-prepend! all-steps-stk (list rep-step))]
                        [else
                         (steps-stack-top-commit-warns! all-steps-stk)
                         (set! all-steps-stk (steps-stack-pop all-steps-stk))]))]
               [else
                (cond
                  ;; no more backtracking point, i.e. the failure is not caused by
                  ;; a skippable (0-repeated-times) repetition form
                  [(steps-stack-last? all-steps-stk)
                   (steps-stack-top-commit-warns! all-steps-stk)
                   ;; record the error but continue to compare new results
                   (warn error-box
                         'test-sequence
                         "steps do not match\n   given: ~v\nexpected: ~v"
                         (show-result result error-box)
                         expected)
                   (steps-stack-top-next! all-steps-stk)
                   (when (null? (steps-stack-top all-steps-stk))
                     ;; test is happy, release the semaphore:
                     (semaphore-post test-finished-semaphore))]
                  ;; the current skippable repetition form failed.
                  ;; start from the backtracking point with all received forms
                  [else
                   (define accumulated-results
                     (steps-stack-top-received-results all-steps-stk))
                   (set! all-steps-stk (steps-stack-pop all-steps-stk))
                   (for-each receive-result accumulated-results)])])]))))
  (define iter-caller
    (λ (init iter)
      (init)
      (call-iter-on-each (expanded-thunk) iter)))
  (let/ec escape
    (parameterize ([error-escape-handler
                    ;; in the real stepper, it's okay to allow an error to terminate the
                    ;; whole computation, because it's running in its own thread.
                    ;; During testing, we want to simply abort out
                    ;; to this escape continuation.
                    (λ ()
                      (semaphore-post test-finished-semaphore)
                      (escape (void)))]
                   [get-rewritten-error-message-parameter get-rewriten-error-message])
      (go iter-caller dynamic-requirer receive-result render-settings
          #:disable-error-handling? (disable-stepper-error-handling))))
  (match (sync/timeout MAX-TEST-WAIT test-finished-semaphore)
    [#f (warn error-box 'test-sequence
              "test engine timeout while waiting for steps")]
    [(? semaphore? s) 'success])
  (error-display-handler current-error-display-handler))

(define-namespace-anchor n-anchor)
;; it seems to be okay to use the same namespace for all of the tests... 
(define test-namespace (make-base-namespace))
;; yikes, this code looks ancient. I'm guessing there's an easier/better way
;; to do this now:
(namespace-attach-module (namespace-anchor->empty-namespace n-anchor)
                         'mzlib/pconvert-prop
                         test-namespace)
(namespace-attach-module (namespace-anchor->empty-namespace n-anchor)
                         'racket/private/promise
                         test-namespace)
(namespace-attach-module (namespace-anchor->empty-namespace n-anchor)
                         'racket/class
                         test-namespace)
(namespace-attach-module (namespace-anchor->empty-namespace n-anchor)
                         'test-engine/racket-tests
                         test-namespace)
(parameterize ([current-namespace test-namespace])
  (namespace-require 'test-engine/test-engine)
  (initialize-test-object!))

;; call-iter-on-each : (-> syntax?) (syntax? (-> 'a) -> 'a) -> void/c
;; call the given iter on each syntax in turn (iter bounces control
;; back to us by calling the followup-thunk).
(define (call-iter-on-each stx-thunk iter)
  (let iter-loop ()
    (define next (stx-thunk))
    (cond [(eof-object? next)
           (iter next void)]
          [else
           ;; I think this expand is unneccessary:
           (iter (expand next) iter-loop)])))


(define (warn error-box who fmt . args)
  (set-box! error-box #t)
  (eprintf "~a: ~a\n" who (apply format fmt args)))


;; (-> step-result? sexp? boolean?)
(define (compare-steps actual expected error-box all-steps-stk)
  (match expected
    [`(before-after ,before ,after)
     (and (Before-After-Result? actual)
          (andmap (lambda (exps expected name)
                    (unless (list? exps)
                      (warn error-box
                            'compare-steps "not a list: ~v"
                            (syntax->hilite-datum exps)))
                    (noisy-equal? (map syntax->hilite-datum
                                       exps)
                                  expected
                                  name
                                  error-box
                                  all-steps-stk))
                  (list (map sstx-s (Before-After-Result-pre-exps actual))
                        (map sstx-s (Before-After-Result-post-exps actual)))
                  (list before after)
                  (list 'before 'after)))]
    [`(error ,err-msg)
     (and (Error-Result? actual)
          (string-contains? (Error-Result-err-msg actual) err-msg))]
    [`(before-error ,before ,err-msg)
     (and (Before-Error-Result? actual)
          (and (noisy-equal? (map syntax->hilite-datum
                                  (map sstx-s (Before-Error-Result-pre-exps actual)))
                             before
                             'before
                             error-box
                             all-steps-stk)
               (equal? err-msg (Before-Error-Result-err-msg actual))))]
    [`(finished-stepping) (eq? 'finished-stepping actual)]
    [`(ignore) (warn error-box 
                     'compare-steps "ignoring one step") #t]
    [else (begin (warn error-box 
                       'compare-steps
                       "unexpected expected step type: ~v" expected)
                 #f)]))



;; used to display results in an error message
(define (show-result r error-box)
  (if (Before-After-Result? r)
      (list 'before-after-result
            (map (lambda (exps)
                   (unless (list? exps)
                     (warn error-box 
                           'show-result "not a list: ~v"
                           (syntax->hilite-datum exps)))
                   (map syntax->hilite-datum
                        exps))
                 (list (map sstx-s (Before-After-Result-pre-exps r))
                       (map sstx-s (Before-After-Result-post-exps r)))))
      r))

;; noisy-equal? : (any any . -> . boolean)
;; like equal?, but prints a noisy error message
(define (noisy-equal? actual expected name error-box all-steps-stk)
  (if (equal? actual expected)
      #t
      (begin (steps-stack-top-warn all-steps-stk error-box 'not-equal?
                   "~.s:\nactual:   ~e =/= \nexpected: ~e\n  here's the diff: ~e" name actual expected (sexp-diff actual expected))
             #f)))

