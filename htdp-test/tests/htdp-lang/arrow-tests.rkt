#lang at-exp racket

;; This module tests binding arrows for code involving "template dots"
;; in the spirit of HtDP templates. ~~ See bottom for illustrations of
;; flaws in the current implementation. 

(require drracket/check-syntax)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define-syntax-rule
  (define-get-arrows get-what-arrows method-header arrow-info) ;; Robby, you write macros like I do
  (define (get-what-arrows str)
    (define-get-arrows/proc
      (λ (%)
        (class %
          (inherit add-item)
          (define/override method-header
            (add-item arrow-info))
          (super-new)))
      str)))

(define (define-get-arrows/proc mixin str)
  (define results '())

  (define annotations%
    (class (annotations-mixin object%)
      (super-new)
      (define/public (add-item x)
        (when x
          (set! results (cons x results))))
      (define/override (syncheck:find-source-object stx)
        (if (eq? 'the-source (syntax-source stx))
            'yep
            #f))))

  (define annotations (new (mixin annotations%)))
  (define-values (add-syntax done) (make-traversal (make-base-namespace) #f))

  (parameterize ([current-annotations annotations]
                 [current-namespace   (make-base-namespace)])
    (add-syntax
     (expand
      (parameterize ([read-accept-reader #t])
        (read-syntax 'the-source (open-input-string str)))))
    (done))
  
  (apply set results))

(define-get-arrows get-binding-arrows
  (syncheck:add-arrow _start-source-obj start-left start-right 
                      _end-source-obj end-left end-right 
                      _actual? 
                      _phase-level)
  (list (list start-left start-right) (list end-left end-right)))

;; ---------------------------------------------------------------------------------------------------
;; The programs define `foo` with parameter `bar` either via `lambda` or `(define (foo bar)`. 
;; They also contain a `(lambda (bar) bar)` and `(local ((define bar 0)` to check nested scope.
;; ASSUME1 Use only `foo` in application position and only once. 
;; ASSUME2 Use only `0` as literal data.

(module+ test

  (define bsl-line  "#lang htdp/bsl\n")
  (define bsl+-line "#lang htdp/bsl+\n")
  (define isl-line  "#lang htdp/isl\n")
  (define isl+-line "#lang htdp/isl+\n")
  (define all-langs (list bsl-line bsl+-line isl-line isl+-line))

  (define lang-px    #px"htdp/.sl(\\+|)")
  (define define-px  #px"define")
  (define local-px   #px"local")
  (define dots-px    #px"\\.\\.\\.")
  (define lambda-px  #px"lambda")
  (define cond-px    #px"cond")
  (define else-px    #px"else")
  (define empty?-px  #px"empty\\?")
  (define data-px    #px"0")      ;; ASSUME2 
  (define foo-app-px #px"\\(foo") ;; ASSUME1
  
  #; {type Bindings = [Setof [List [List N N] [List N N]]]}

  #; {String Bindings [#:scoped String Bindings -> Bindings] -> Void}
  (define (test program-body app-selector #:scoped (scoped (λ (_ se) se)))
    (λ (lang-line)
      (define program (string-append lang-line (~a program-body)))
      (define expected (scoped program (standard-arrows lang-line app-selector program)))
      (check-equal? (get-binding-arrows program) expected program)))

  #; {String Bindings -> Bindings}
  (define (nested-scope program standard-expected)
    (define bar-> (drop (map cons->list (regexp-match-positions* #px"bar" program)) 2))
    (set-add standard-expected bar->))
  
  #; {String [ [Listof X] -> X ] String -> Bindings}
  ;; arrows from #lang to app, lambbda, dots; 
  ;; arrows from first occurrence of foo and bar to themselves, respectively
  (define (standard-arrows lang-line app-selector program)
    (define lang (cons->list (first (regexp-match-positions lang-px program))))
    
    (define posn->arrow-list (posn->arrow lang list))
    (define ->def (map posn->arrow-list (regexp-match-positions* define-px program)))
    (define ->loc (map posn->arrow-list (regexp-match-positions* local-px  program)))
    (define ->lam (map posn->arrow-list (regexp-match-positions* lambda-px program)))
    (define ->con (map posn->arrow-list (regexp-match-positions* cond-px program)))
    (define ->els (map posn->arrow-list (regexp-match-positions* else-px program)))
    (define ->mt? (map posn->arrow-list (regexp-match-positions* empty?-px program)))
    (define ->dot (map posn->arrow-list (regexp-match-positions* dots-px   program)))

    (define posn->arrow-0span (posn->arrow lang 0span))
    (define ->data (map posn->arrow-0span (regexp-match-positions* data-px program)))
    ;; function applications in BSL* are actually macro uses; eliminate `(define (foo x)` too
    (define apps (app-selector (regexp-match-positions* foo-app-px program)))
    (define ->app (if (regexp-match #px"bsl" lang-line) '[] (map posn->arrow-0span apps)))

    (define lang-> (append ->dot ->loc ->lam ->mt? ->els ->con ->def #;->data ->app))

    ;; positions as arrows (first two occurrences; let specific examples deal with others)
    (define foo-> (map cons->list (take (regexp-match-positions* #px"foo" program) 2)))
    (define bar-> (map cons->list (take (regexp-match-positions* #px"bar" program) 2)))
    
    (apply set (list* foo-> bar-> lang->)))
  
  (define ((posn->arrow lang sel) x) (list lang (cons->list x sel)))
  (define (0span l _r) (list l l))
  (define (cons->list x (sel list)) (sel (car x) (cdr x)))

  ;; now test:

  (define program-body0 ;; the most basic example 
    '(define (foo bar)
       (... (foo bar))))
  
  (define program-body1 ;; Sam's "broken syntax for templates" example
    '(define (foo bar)
       (... 
        (cond
          [(empty? l) ... (foo bar) ...]
          [else 0]))))

  (define program-body2 ;; even in BSL, instructors can use `lambda` 
    '(define foo
       (lambda (bar)
         (... (foo bar)))))

  ;; TO DEMONSTRATE THE FAILURES OF THE CURRENT APPROACH:
  ;; expected to fail because turning syntax-arrows into binding arrows
  ;; is going to overlook nested scope.
  
  (define program-body3 ;; using `lambda` to create a nested scope 
    '(define foo
       (lambda (bar)
         (... (foo bar (lambda (bar) bar))))))

  (define program-body4 ;; using `local` to create a nested scope
    '(define foo
       (lambda (bar)
         (... (foo bar (local ((define bar 0)) bar))))))
  
  (for-each (test program-body0 (λ _ '[])) all-langs)
  (for-each (test program-body1 (λ _ '[]) #;(compose list first)) all-langs)
  (for-each (test program-body2 rest) all-langs))

;; The following tests are expected to fail. They represent cases where the current implementation
;; goes wrong for the sake of backwards compatibility for teaching material.

#; 
(module+ test
  ((test program-body3 rest #:scoped nested-scope) isl+-line)
  (for-each (test program-body4 values #:scoped nested-scope) (list isl-line isl+-line)))
