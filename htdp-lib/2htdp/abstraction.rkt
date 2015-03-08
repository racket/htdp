#lang racket 

(provide 
 ;; SYNTAX: expression 
 ;; (for[*]/xyz ([x0:id range0:expr] [x:id range:expr] ...) body:exp)
 
 (rename-out 
  (for/list0 for/list)       (for*/list0 for*/list)
  (for/and0 for/and)         (for*/and0 for*/and)
  (for/or0 for/or)           (for*/or0 for*/or)
  (for/sum0 for/sum)         (for*/sum0 for*/sum)
  (for/product0 for/product) (for*/product0 for*/product))
 
 ;; -> Sequence 
 in-naturals
 
 ;; SYNTAX: definition 
 ;; (define-type name (name name) ...)
 ;; (rename-out (define-type0 define-type))
 define-type
 
 ;; SYNTAX: expression 
 ;; (type-case case:expr (name (name ...) body:expr) ...)
 (rename-out (type-case0 type-case)))

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse) syntax/parse plai)

(begin-for-syntax
  (define-syntax-class comprehension-clause
    #:description "comprehension clause"
    #:attributes (x range)
    (pattern (x:id range:expr))))

;; restrict for/xyz to simple form and name it for/xyz0
(define-syntax (define-for stx)
  (syntax-parse stx 
    [(_ for/xyz0:id for/xyz:id)
     #'(define-syntax (for/xyz0 stx)
         (syntax-parse stx
           [(_ (clause0:comprehension-clause clause:comprehension-clause (... ...)) body:expr)
            #`(for/xyz ((clause0.x clause0.range) (clause.x clause.range) (... ...)) 
                       ;; the following line exists only so that coverage doesn't hilite x0 x ...
                       clause0.x clause.x (... ...) 
                       body)]
           [(_ () body:expr)
            (define clauses (cadr (syntax->list stx)))
            (define message "expected at least one comprehension clause, found none")
            (raise-syntax-error #f message stx clauses)]))]))

(define-for for/list0 for/list)
(define-for for*/list0 for*/list)

(define-for for/and0 for/and)
(define-for for*/and0 for*/and)

(define-for for/or0 for/or)
(define-for for*/or0 for*/or)

(define-for for/sum0 for/sum)
(define-for for*/sum0 for*/sum)

(define-for for/product0 for/product)
(define-for for*/product0 for*/product)

#;
(define-syntax (define-type0 stx)
  (syntax-parse stx 
    [(_ type:id (variant:id (field:id predicate:id) ...) ...)
     #'(define-type type (variant (field predicate) ...) ...)]))

(define-syntax (type-case0 stx)
  (syntax-parse stx 
    [(_ type:id case:expr [variant:id (field:id ...) body:expr] ...)
     #'(type-case type case [variant (field ...) body] ...)]))