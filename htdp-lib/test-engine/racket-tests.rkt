; Surface syntax for check-expect & friends in Racket-like languages.
#lang racket/base

(provide check-expect ;; syntax : (check-expect <expression> <expression>)
         check-random ;; syntax : (check-random <expression> <expression>)
         check-within ;; syntax : (check-within <expression> <expression> <expression>)
         check-member-of ;; syntax : (check-member-of <expression> <expression>)
         check-range ;; syntax : (check-range <expression> <expression> <expression>)
         check-error  ;; syntax : (check-error <expression> [<expression>])
         check-satisfied ;; syntax : (check-satisfied <expression> <expression>)
         ;; re-exports from test-engine/syntax
         test-execute test-silence
         test) 

(require lang/private/teachprims
         racket/match
         ; racket/function
         htdp/error
         (for-syntax racket/base
                     #;"requiring from" lang/private/firstorder #;"avoids load cycle")
         test-engine/test-engine
         (only-in test-engine/test-markup get-rewritten-error-message)
         test-engine/syntax)

(define INEXACT-NUMBERS-FMT
  "check-expect cannot compare inexact numbers. Try (check-within test ~a range).")
(define FUNCTION-FMT
  "check-expect cannot compare functions.")
(define SATISFIED-FMT
  "check-satisfied: expects function of one argument in second position. Given ~a")
(define CHECK-ERROR-STR-FMT
  "check-error: expects a string (the expected error message) for the second argument. Given ~s")
(define CHECK-WITHIN-INEXACT-FMT
  "check-within: expects an inexact number for the range. ~a is not inexact.")
(define CHECK-WITHIN-FUNCTION-FMT
  "check-within cannot compare functions.")
(define LIST-FMT
  "check-member-of: expects a list for the second argument (the possible outcomes). Given ~s")
(define CHECK-MEMBER-OF-FUNCTION-FMT
  "check-member-of: cannot compare functions.")
(define RANGE-MIN-FMT
  "check-range: expects a number for the minimum value. Given ~a")
(define RANGE-MAX-FMT
  "check-range: expects a number for the maximum value. Given ~a")
(define CHECK-RANGE-FUNCTION-FMT
  "check-range: cannot compare functions.")


(define-for-syntax CHECK-EXPECT-DEFN-STR
  "found a test that is not at the top level")
(define-for-syntax CHECK-WITHIN-DEFN-STR
  CHECK-EXPECT-DEFN-STR)
(define-for-syntax CHECK-ERROR-DEFN-STR
  CHECK-EXPECT-DEFN-STR)

(define-for-syntax (check-context! kind message stx)
  (let ([c (syntax-local-context)])
    (unless (memq c '(module top-level module-begin))
      (raise-syntax-error kind message stx))))

; note that rewrite-error-message might be sensitive to the exact format here
(define-for-syntax (argcount-error-message/stx arity stx [at-least #f])
  (let* ((ls (syntax->list stx))
         (found (if ls (sub1 (length ls)) 0))
         (fn-is-large (> arity found)))
    (format "expects ~a~a~a argument~a, but found ~a~a"
            (if at-least "at least " "")
            (if (or (= arity 0) fn-is-large) "" "only ")
            (if (= arity 0) "no" arity) (if (> arity 1) "s" "")
            (if (and (not (= found 0)) fn-is-large) "only " "")
            (if (= found 0) "none" found))))

(define (make-exn->unexpected-error src expected)
  (lambda (exn)
    (unexpected-error src expected exn)))

(define-syntax (check-expect stx)
  (check-context! 'check-expect CHECK-EXPECT-DEFN-STR stx)
  (syntax-case stx ()
    [(_ test expected)
     (check-expect-maker stx #'do-check-expect #`test (list #`expected) 'comes-from-check-expect)]
    [_ (raise-syntax-error 'check-expect (argcount-error-message/stx 2 stx) stx)]))

(define (do-check-expect test expected src)
  (error-check (lambda (v) (if (number? v) (exact? v) #t))
               expected INEXACT-NUMBERS-FMT #t)
  (error-check (lambda (v) (not (procedure? v)))
               expected FUNCTION-FMT #f)
  (execute-test
   src
   (lambda ()
     (let ((actual (test)))
       (if (teach-equal? actual expected)
           #t
           (unequal src actual expected))))
   (make-exn->unexpected-error src expected)))

(define-syntax (check-random stx)
  (syntax-case stx ()
    [(check-random e1 e2)
     (let ([test #`(lambda () e1)]
           [expected (list #`(lambda () e2))])
       (check-expect-maker stx #'do-check-random test expected 'comes-from-check-random))]
    [_ (raise-syntax-error 'check-random (argcount-error-message/stx 2 stx) stx)]))

(define (do-check-random test expected-thunk src)
  (let ((rng (make-pseudo-random-generator))
        (k (modulo (current-milliseconds) (sub1 (expt 2 31)))))
    (let ((expected (parameterize ([current-pseudo-random-generator rng])
                          (random-seed k)
                          (expected-thunk))))
      (error-check (lambda (v) (if (number? v) (exact? v) #t))
                   expected INEXACT-NUMBERS-FMT #t)
      (execute-test
       src
       (lambda ()
         (let ((actual (parameterize ([current-pseudo-random-generator rng])
                         (random-seed k)
                         ((test)))))
           (if (teach-equal? actual expected)
               #t
               (unequal src actual expected))))
       (make-exn->unexpected-error src expected)))))

(define-syntax (check-satisfied stx)
  (syntax-case stx ()
    [(_ actual:exp expected-predicate:id)
     (identifier? #'expected-predicate:id)
     (let* ([prop1 (first-order->higher-order #'expected-predicate:id)]
            [name (symbol->string (syntax-e  #'expected-predicate:id))]
            [code #`(lambda (x)
                      (with-handlers ([exn:fail:contract:arity?
                                       (lambda (exn)
                                         (let* ((msg (exn-message exn))
                                                (msg1 (regexp-match #px"(.*): arity mismatch" msg)))
                                           (cond
                                             [msg1
                                              (let ((raised-name (cadr msg1)))
                                                (if (equal? #,name raised-name)
                                                    (error-check (lambda (v) #f) #,name SATISFIED-FMT #t)
                                                    (raise exn)))]
                                             [else (raise exn)])))])
                        (#,prop1 x)))])
       (check-expect-maker stx 
                           #'do-check-satisfied 
                           #'actual:exp
                           (list code name)
                           'comes-from-check-satisfied))]
    [(_ actual:exp expected-predicate:exp)
     (let ([pred #`(let ([p? expected-predicate:exp])
                     (let ((name (object-name p?)))
                       (unless (and (procedure? p?) (procedure-arity-includes? p? 1))
                         (if name  ;; this produces the BSL/ISL name 
                             (error-check (lambda (v) #f) name SATISFIED-FMT #t)
                             (error-check (lambda (v) #f) p? SATISFIED-FMT #t))))
                     p?)])
       (check-expect-maker stx 
                           #'do-check-satisfied
                           #'actual:exp
                           (list pred "unknown name")
                           'comes-from-check-satisfied))]
    [(_ actual:exp expected-predicate:exp) 
     (raise-syntax-error 'check-satisfied "expects named function in second position." stx)]
    [_ (raise-syntax-error 'check-satisfied (argcount-error-message/stx 2 stx) stx)]))

(define (do-check-satisfied test p? name src)
  (execute-test
   src
   (lambda ()
     (unless (and (procedure? p?) (procedure-arity-includes? p? 1))
       (error-check (lambda (v) #f) name SATISFIED-FMT #t))
     (let* ((actual (test))
            (ok? (p? actual)))
       (cond
         [(not (boolean? ok?))
          ;; (error-check (lambda (v) #f) name "expected a boolean" #t)
          (check-result (format "~a [as predicate in check-satisfied]" name) boolean? "boolean" ok?)]
         [(not ok?)
          (satisfied-failed src actual name)]
         [else #t])))
   (lambda (exn)
     (unsatisfied-error src name exn))))

(define-syntax (check-within stx)
  (check-context! 'check-within CHECK-WITHIN-DEFN-STR stx)
  (syntax-case stx ()
    [(_ test expected within)
     (check-expect-maker stx #'do-check-within #`test (list #`expected #`within) 
                         'comes-from-check-within)]
    [_ (raise-syntax-error 'check-within (argcount-error-message/stx 3 stx) stx)]))

(define (do-check-within test expected within src)
  (error-check number? within CHECK-WITHIN-INEXACT-FMT #t)
  (error-check (lambda (v) (not (procedure? v))) expected CHECK-WITHIN-FUNCTION-FMT #f)
  (execute-test
   src
   (lambda ()
     (let ((actual (test)))
       (if (beginner-equal~? actual expected within)
           #t
           (not-within src actual expected within))))
   (make-exn->unexpected-error src expected)))

(define-syntax (check-error stx)
  (check-context! 'check-error CHECK-ERROR-DEFN-STR stx)
  (syntax-case stx ()
    [(_ test error)
     (check-expect-maker stx #'do-check-error #`test (list #`error)
                         'comes-from-check-error)]
    [(_ test)
     (check-expect-maker stx #'do-check-error/no-message #`test '()
                         'comes-from-check-error)]
    [(_) (raise-syntax-error 'check-error (argcount-error-message/stx 1 stx #t) stx)]
    [_ (raise-syntax-error 'check-error (argcount-error-message/stx 2 stx) stx)]))

(define (do-check-error test error src)
  (error-check string? error CHECK-ERROR-STR-FMT #t)
  (execute-test
   src
   (lambda ()
     (with-handlers ([exn?
                      (lambda (exn)
                        (let ((msg (get-rewritten-error-message exn)))
                          (if (equal? msg error)
                              #t
                              (incorrect-error src error exn))))])
       (let ([actual (test)])
         (expected-error src error actual))))
   (make-exn->unexpected-error src error))) ; probably can't happen

(define (do-check-error/no-message test src)
  (execute-test
   src
   (lambda ()
     (with-handlers ([exn?
                      (lambda (exn) #t)])
       (let ([actual (test)])
         (expected-error src #f actual))))
   (make-exn->unexpected-error src "any error"))) ; probably can't happen

(define-syntax (check-member-of stx)
  (check-context! 'check-member-of CHECK-EXPECT-DEFN-STR stx)
  (syntax-case stx ()
    [(_ test expected expecteds ...)
     (check-expect-maker stx
                         #'do-check-member-of
                         #`test
                         (list #`(list expected expecteds ...))
                         'comes-from-check-member-of)]
    [_ (raise-syntax-error 'check-member-of (argcount-error-message/stx 2 stx #t) stx)]))

(define (do-check-member-of test expecteds src)
  (for-each
   (lambda (expected)
     (error-check (lambda (v) (not (procedure? v))) expected CHECK-MEMBER-OF-FUNCTION-FMT #f))
   expecteds)
  (execute-test
   src
   (lambda ()
     (let ((actual (test)))
       (if (memf (lambda (expected) (teach-equal? actual expected)) expecteds)
           #t
           (not-mem src actual expecteds))))
   (make-exn->unexpected-error src expecteds)))
       
(define-syntax (check-range stx)
  (check-context! 'check-member-of CHECK-EXPECT-DEFN-STR stx)
  (syntax-case stx ()
    [(_ test min max)
     (check-expect-maker stx #'do-check-range #`test (list #`min #`max)
                         'comes-from-check-range)]
    [_ (raise-syntax-error 'check-range (argcount-error-message/stx 3 stx) stx)]))

(define (do-check-range test min max src)
  (error-check number? min RANGE-MIN-FMT #t)
  (error-check number? max RANGE-MAX-FMT #t) 
  (error-check (lambda (v) (not (procedure? v))) min CHECK-RANGE-FUNCTION-FMT #f)
  (error-check (lambda (v) (not (procedure? v))) max CHECK-RANGE-FUNCTION-FMT #f)
  (execute-test
   src
   (lambda ()
     (let ((val (test)))
       (if (and (number? val)
                (<= min val max))
           #t
           (not-range src val min max))))
   (make-exn->unexpected-error src (format "[~a, ~a]" min max))))

(define (error-check pred? actual fmt fmt-act?)
  (unless (pred? actual)
    (raise
     (make-exn:fail:contract (if fmt-act? (format fmt actual) fmt)
                             (current-continuation-marks)))))


