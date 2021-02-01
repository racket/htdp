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
;; ASSUME2 USE only `0` as literal data. 

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

  (define (make-program lang-line body)
    (string-append lang-line body))

  (define (cons->list x) (match x [(cons a b) (list a b)]))

  #; "lang htdp/*sl*"
  #; (define (foo bar) (... (foo bar)))
  (define ((test1 app-selector) lang-line)
    (define program-body
      @string-append{
 (define (foo bar)
 (... (foo bar))) })
    (define program (make-program lang-line program-body))
    (define expected (standard-arrows lang-line app-selector program))
    (check-equal? (get-binding-arrows program) expected lang-line))

  #; "lang htdp/*sl*"
  #; (define foo (lambda (bar) (... (foo bar))))
  (define ((test2 app-selector) lang-line)
    (define program-body
      @string-append{
 (define foo
 (lambda (bar)
 (... (foo bar))))})
    (define program (make-program lang-line program-body))
    (define expected (standard-arrows lang-line app-selector program))
    (check-equal? (get-binding-arrows program) expected (string-append "lambda" lang-line)))

  #; "lang htdp/isl+"
  #; (define foo (lambda (bar) (... (foo bar (lambda (bar) bar)))))
  (define (test3 app-selector)
    (define program-body
      @string-append{
 (define foo
 (lambda (bar)
 (... (foo bar (lambda (bar) bar)))))})
    (define program (make-program isl+-line program-body))
    (define standard-expected (standard-arrows isl+-line app-selector program))
  
    (define bar-> (drop (map cons->list (regexp-match-positions* #px"bar" program)) 2))
    (define expected (set-add standard-expected bar->))
  
    (check-equal? (get-binding-arrows program) expected "test 3"))

  #; "lang htdp/isl+"
  #; (define foo (lambda (bar) (... (foo bar (lambda (bar) bar)))))
  (define ((test4 app-selector) isl+-line)
    (define program-body
      @string-append{
 (define foo
 (lambda (bar)
 (... (foo bar (local ((define bar 0)) bar)))))})
    (define program (make-program isl+-line program-body))
    (define standard-expected (standard-arrows isl+-line app-selector program))
  
    (define bar-> (drop (map cons->list (regexp-match-positions* #px"bar" program)) 2))
    (define expected (set-add standard-expected bar->))
  
    (check-equal? (get-binding-arrows program) expected "test 4"))

  #; {String [ [Listof X] -> X ] String -> [Setof [List [List N N] [List N N]]]}
  ;; arrows from #lang to app, lambbda, dots; 
  ;; arrows from first occurrence of foo and bar to themselves, respectively
  (define (standard-arrows lang-line app-selector program)
    ;; positions
    (define lang (first (map cons->list (regexp-match-positions lang-px program))))
    (define defi (map cons->list (regexp-match-positions* define-px program)))
    (define loca (map cons->list (regexp-match-positions* local-px program)))
    (define dots (map cons->list (regexp-match-positions dots-px program)))
    (define lamb (map cons->list (regexp-match-positions* lambda-px program)))
    (define apps (app-selector (regexp-match-positions* foo-app-px program)))
    (define data (regexp-match-positions* data-px program))
  
    ;; positions as arrows (two occurrences)
    (define foo-> (take (map cons->list (regexp-match-positions* #px"foo" program)) 2))
    (define bar-> (take (map cons->list (regexp-match-positions* #px"bar" program)) 2))

    (define ->lamb (map (curry list lang) lamb))
    (define ->defi (map (curry list lang) defi))
    (define ->loca (map (curry list lang) loca))
    (define ->dots (cons lang dots))
    (define ->app  (map (λ (a) (list lang (list (car a) (car a)))) apps))
    (define ->data (map (λ (a) (list lang (list (car a) (car a)))) data))

    (define the-list (list* ->dots foo-> bar-> (append ->loca ->lamb ->defi ->data)))
    (apply set (append the-list (if (regexp-match #px"bsl" lang-line) '[] ->app))))

  (for-each (test1 rest) all-langs)
  (for-each (test2 values) all-langs)
  (test3 values)
  (for-each (test4 values) (list isl-line isl+-line)))