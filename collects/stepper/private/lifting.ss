(module find-highlight mzscheme
  (require "highlight-placeholder.ss"
           (lib "etc.ss")
           (lib "contracts.ss")
           (prefix kernel: (lib "kerncase.ss" "syntax"))) 
  
  (define-struct context-record (stx index kind))

  (provide/contract [find-highlight (-> syntax? (listof context-record?))])
  
  (define (find-highlight stx)
    (let/ec success-escape
      (local
          ((define (make-try-all-subexprs stx kind context-so-far)
             (lambda (index-mangler list-of-subtries)
               (let loop ([index 0] [remaining list-of-subtries])
                 (unless (null? remaining)
                   (fprintf (current-output-port) "~v\n" (syntax-object->datum stx))
                   (if (kernel:kernel-syntax-case stx #f 
                         [(#%top . var)
                          (eq? (syntax-e #'var) highlight-placeholder)
                          #t]
                         [else #f]) ;(eq? stx highlight-placeholder)
                       (begin
                         (fprintf (current-error-port) "success!\n")
                       (success-escape context-so-far))
                       (begin
                         ((caar remaining) (cadar remaining) (cons (make-context-record stx (index-mangler index) kind) context-so-far))
                         (loop (+ index 1) (cdr remaining))))))))
           
           (define try->offset-try
             (lambda (try)
               (lambda (offset subtries)
                 (try (lambda (index) (list (+ offset index))) subtries))))
           
           (define (top-level-expr-iterator stx context-so-far)
             (let ([try (try->offset-try (make-try-all-subexprs stx 'top-level context-so-far))])
               (kernel:kernel-syntax-case stx #f
                 [(module identifier name (#%plain-module-begin . module-level-exprs))
                  (try 3 (map (lambda (expr) `(,module-level-expr-iterator ,expr))
                              (syntax->list #'module-level-exprs)))]
                 [else-stx
                  (general-top-level-expr-iterator stx context-so-far)])))
           
           (define (module-level-expr-iterator stx context-so-far)
             (kernel:kernel-syntax-case stx #f
               [(provide . provide-specs)
                (void)]
               [else-stx
                (general-top-level-expr-iterator stx context-so-far)]))
           
           (define (general-top-level-expr-iterator stx context-so-far)
             (let ([try (try->offset-try (make-try-all-subexprs stx 'general-top-level context-so-far))])
               (kernel:kernel-syntax-case stx #f
                 [(define-values (var ...) expr)
                  (try 2 `((,expr-iterator ,#'expr)))]
                 [(define-syntaxes (var ...) expr)
                  (try 2 `((,expr-iterator ,#'expr)))]
                 [(begin . top-level-exprs)
                  (try 1 (map (lambda (expr) `(,top-level-expr-iterator ,expr))
                              (syntax->list #'exprs)))]
                 [(require . require-specs)
                  (void)]
                 [(require-for-syntax . require-specs)
                  (void)]
                 [else
                  (expr-iterator stx context-so-far)])))
           
           (define (expr-iterator stx context-so-far)
             (let* ([try (make-try-all-subexprs stx 'expr context-so-far)]
                    [try-exprs (lambda (index-mangler exprs) (try index-mangler (map (lambda (expr) `(,expr-iterator ,expr)) (syntax->list exprs))))]
                    [try-exprs-offset (try->offset-try try-exprs)] 
                    [let-values-abstraction
                     (lambda (stx)
                       (kernel:kernel-syntax-case stx #f
                         [(kwd (((variable ...) rhs) ...) . bodies)
                          (begin
                            (try-exprs (lambda (index) (list 1 index 1)) #'(rhs ...))
                            (try-exprs-offset 2 #'bodies))]
                         [else
                          (error 'expr-syntax-object-iterator 
                                 "unexpected let(rec) expression: ~a"
                                 (syntax-object->datum stx))]))]) 
               (kernel:kernel-syntax-case stx #f
                 [var-stx
                  (identifier? (syntax var-stx))
                  (void)]
                 [(lambda vars . bodies)
                  (try-exprs-offset 2 #'bodies)]
                 [(case-lambda (vars . bodies) ...)
                  (let loop ([count 1] [clauses (syntax->list #'(bodies ...))])
                    (try-exprs (lambda (index) (list count (+ index 1))) (car clauses)))]
                 [(if test then)
                  (try-exprs-offset 1 #'(test then))]
                 [(if test then else)
                  (try-exprs-offset 1 #'(test then else))]
                 [(begin . bodies)
                  (try-exprs-offset 1 #'bodies)]
                 [(begin0 . bodies)
                  (try-exprs-offset 1 #'bodies)]
                 [(let-values . _)
                  (let-values-abstraction stx)]
                 [(letrec-values . _)
                  (let-values-abstraction stx)]
                 [(set! var val)
                  (try-exprs-offset 2 #'(val))]
                 [(quote _)
                  (void)]
                 [(quote-syntax _)
                  (void)]
                 [(with-continuation-mark key mark body)
                  (try-exprs-offset 1 #'(key mark body))]
                 [(#%app . exprs)
                  (begin
                    (fprintf (current-error-port) "application sub-exprs: ~v\n" (map syntax-object->datum (syntax->list #'exprs)))
                  (try-exprs-offset 0 #'exprs))]
                 [(#%datum . _)
                  (void)]
                 [(#%top . var)
                  (void)]
                 [else
                  (error 'expr-syntax-object-iterator "unknown expr: ~a" 
                         (syntax-object->datum stx))]))))
        
        (if (eq? stx highlight-placeholder)
            null
            (top-level-expr-iterator stx null)))))
  
  )

(require find-highlight
         "highlight-placeholder.ss"
         (lib "kerncase.ss" "syntax"))

(define (datum-ize-context-record cr)
  (make-context-record (syntax-object->datum (context-record-stx cr))
                       (context-record-index cr)
                       (context-recond-kind cr)))

(map datum-ize-context-record
  (find-highlight (expand #`(define (f x) (local ((define (a x) (b (- x 1))) (define (b x) (#,highlight-placeholder x))) (a x))))))

(list (make-context-record `(,highlight-placeholder x) 'expr '(0)) 
      (make-context-record `(letrec ([a (lambda (x) (b (- x 1)))] [b (lambda (x) (,highlight-placeholder x))]) (a x)) '(1 1 1) 'expr)
      (make-context-record `(define-values (f x) (letrec ([a (lambda (x) (b (- x 1)))] [b (lambda (x) (,highlight-placeholder x))]) (a x))) '(2) 'general-top-level-expr))
