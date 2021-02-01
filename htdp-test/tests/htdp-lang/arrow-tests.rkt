#lang at-exp racket

;; This module tests binding arrows for code involving "template dots". 

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

;; This is not needed.
#;
(define-get-arrows get-binding-arrows/pxpy
  (syncheck:add-arrow/name-dup/pxpy start-source-obj    
                                    start-left  
                                    start-right
                                    start-px
                                    start-py
                                    end-source-obj      
                                    end-left    
                                    end-right
                                    end-px
                                    end-py
                                    actual?     
                                    phase-level
                                    require-arrows?
                                    name-dup?)
  (list (list start-left start-right start-px start-py)
        (list end-left end-right end-px end-py)))

;; ---------------------------------------------------------------------------------------------------
;; The programs define `foo` with parameter `bar` either via `lambda` or `(define (foo bar)`. 
;; They also contain a `(lambda (bar) bar)` and `(local ((define bar 0)` to check nested scope.
;; ASSUME1 Use only `foo` in application position.
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
    (define ->dot (map posn->arrow-list (regexp-match-positions* dots-px   program)))

    (define posn->arrow-0span (posn->arrow lang 0span))
    (define ->data (map posn->arrow-0span (regexp-match-positions* data-px program)))
    ;; function applications in BSL* are actually macro uses; eliminate `(define (foo x)` too
    (define apps  (app-selector (regexp-match-positions* foo-app-px program)))
    (define ->app (if (regexp-match #px"bsl" lang-line) '[] (map posn->arrow-0span apps)))

    (define lang-> (append ->dot ->loc ->lam ->def ->data ->app))

    ;; positions as arrows (first two occurrences; let specific examples deal with others)
    (define foo-> (map cons->list (take (regexp-match-positions* #px"foo" program) 2)))
    (define bar-> (map cons->list (take (regexp-match-positions* #px"bar" program) 2)))
    
    (apply set (list* foo-> bar-> lang->)))
  
  (define ((posn->arrow lang sel) x) (list lang (cons->list x sel)))
  (define (0span l _r) (list l l))
  (define (cons->list x (sel list)) (sel (car x) (cdr x)))

  ;; now test:

  (define program-body0
    '(define (foo bar)
       (... (foo bar))))
  
  (define program-body1
    '(define (foo bar)
       (... (foo bar))))

  (define program-body2
    '(define foo
       (lambda (bar)
         (... (foo bar)))))

  (define program-body3
    '(define foo
       (lambda (bar)
         (... (foo bar (lambda (bar) bar))))))

  (define program-body4
    '(define foo
       (lambda (bar)
         (... (foo bar (local ((define bar 0)) bar))))))
  
  (for-each (test program-body0 rest) all-langs)
  (for-each (test program-body1 rest) all-langs)
  (for-each (test program-body2 values) all-langs)
  ((test program-body3 values #:scoped nested-scope) isl+-line)
  (for-each (test program-body4 values #:scoped nested-scope) (list isl-line isl+-line)))