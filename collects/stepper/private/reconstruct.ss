(module reconstructor mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (prefix utils: "utils.ss")
           "marks.ss"
           (prefix model: "model.ss")
           "shared.ss"
           "my-macros.ss")

  (provide
   reconstruct-completed
   reconstruct-current
   final-mark-list?
   skip-result-step?
   skip-redex-step?)
  
  
  ; copied from annotater.ss:
  
  ; a BINDING is a syntax-object
  ; a VARREF is a syntax-object
  
  ; a BINDING-SET is (union 'all (listof BINDING))
  ; a VARREF-SET is (listof VARREF)
  
  (make-contract-checker BINDING-SET
                         (lambda (arg)
                           (or (eq? arg 'all)
                               (andmap identifier? arg))))
  (make-contract-checker VARREF-SET
                         (lambda (arg)
                           (and (list? arg)
                                (andmap identifier? arg))))
  
  (make-contract-checker BOOLEAN boolean?)
  (make-contract-checker SYNTAX-OBJECT syntax?)
  
;(unit/sig stepper:reconstruct^
;  (import [z : zodiac^]
;          [utils : stepper:cogen-utils^]
;          stepper:marks^
;          [s : stepper:model^]
;	  stepper:shared^)

  (define the-undefined-value (letrec ([x x]) x))
  
  (define nothing-so-far (gensym "nothing-so-far-"))
  
;  (define memoized-read->raw
;    (let ([table (make-hash-table-weak)])
;      (lambda (read)
;        (or (hash-table-get table read (lambda () #f))
;            (let ([raw (z:sexp->raw read)])
;              (hash-table-put! table read raw)
;              raw)))))
;  
;  (define (make-apply-pred-to-raw pred)
;    (lambda (expr)
;      (pred (memoized-read->raw (expr-read expr)))))
;             
;  (define (make-check-raw-first-symbol symbol)
;    (make-apply-pred-to-raw
;     (lambda (raw)
;       (and (pair? raw)
;            (eq? (car raw) symbol)))))

  ; split-list : ('a -> boolean) (listof 'a) -> (2vals (listof 'a) (listof 'a))
  ; split-list splits a list into two lists at the first element s.t. (fn element) => true).
  ; that is, split-list yields the lists A and B such that (append A B) gives the original
  ; list, and (fn element) => false for all elements in A, and B is either empty or
  ; (fn (car B)) => true
  
 (define (split-list fn lst)
    (let loop ([remaining lst] [so-far null]) 
      (cond [(null? remaining)
             (2vals (reverse so-far) null)]
            [else
             (if (fn (car remaining))
                 (2vals (reverse so-far) remaining)
                 (loop (cdr remaining) (cons (car remaining) so-far)))])))
  
  ; test cases
  ; (test (2vals '(93 4 2) '(0 2 1)) split-list (lambda (x) (= x 0)) '(93 4 2 0 2 1))
  ; (test (2vals '(3 4 5) '()) split-list (lambda (x) (= x 0)) '(3 4 5))
        
  ; n-split-list : num ('a list) -> ('a list) ('a list)
  ; n-split-list splits a given list A into two lists B and C, such that B contains the
  ; first n elements of A, and C contains the rest.

  (define (n-split-list num lst)
    (let loop ([count num] [remaining lst] [so-far null])
      (if (= count 0)
          (2vals (reverse so-far) remaining)
          (loop (- count 1) (cdr remaining) (cons (car remaining) so-far)))))
  
  ; test cases
  (test (2vals '(a b c) '(d e f)) n-split-list 3 '(a b c d e f))

  ; multi-append : (('a list) list) -> ('a list)
  ; applies append to a bunch of lists.  Would be trivial, except I'm trying to avoid
  ; using 'apply'.
  
  (define (multi-append lol)
    (foldl (lambda (a b) (append b a)) null))
  
  ; reshape-list : ('a list) ('b sexp) -> ('a tree)
  ; reshape-list produces a new sexp with the shape of <template> whose in-order leaf traversal
  ; is <source>.
  
  (define (reshape-list source template)
    (let loop ([template template])
      (cond [(or (pair? template)
                 (null? template))
             (map loop template)]
            (else
             (let ([first (car source)])
               (set! source (cdr source))
               first)))))
  
  ; test cases
  ; (test '((1 2 3) (4 5 6)) reshape-list '(1 2 3 4 5 6) '((a b c) (d e f)))
  ; (test '(() () 1 (2 3) ()) reshape-list '(1 2 3) '(() () a (b c) ()))
  
  ; recon-value : value -> syntax-object
  ; recon-value print-converts a value.  If the value is a closure, rectify-value
  ; prints the name attached to the procedure, unless we're on the right-hand-side
  ; of a let, or unless there _is_ no name.
  
  (define (recon-value val . hint-list)
    (let ([hint (if (pair? hint-list) (car hint-list))]
          [closure-record (closure-table-lookup val (lambda () #f))])
      (cond
        [closure-record
         (cond [(and (not (eq? hint 'let-rhs))
                     (closure-record-name closure-record)) =>
                (lambda (name)
                  (cond [(closure-record-lifted-name closure-record) =>
                         (lambda (lifted-name)
                           (d->so (construct-lifted-name name lifted-name)))]
                        [else (d->so name)]))]
               [else
                (let ([mark (closure-record-mark closure-record)])
                  (recon-source-expr (mark-source mark) (list mark)))])]
        [else
         (d->so (s:print-convert val))])))
  
  (define (let-rhs-recon-value val)
    (recon-value val 'let-rhs))
  
  (define (final-mark-list? mark-list)
    (and (not (null? mark-list)) (eq? (mark-label (car mark-list)) 'final)))
 
  (define continuation? 
    (let ([r (regexp "#<continuation>")])
      (lambda (k)
        (let ([p (open-output-string)])
          (display k p)
          (not (not (regexp-match r (get-output-string p))))))))
  
  (define (skip-result-step? mark-list)
    (in-inserted-else-clause mark-list))
  
  (define (skip-redex-step? mark-list)
    (and (pair? mark-list)
         (let ([expr (mark-source (car mark-list))])
           (or (in-inserted-else-clause mark-list) 
               (kernel:kernel-syntax-case expr #f
                  [id
                   (identifier? expr)
                   (or (eq? (syntax-property expr 'stepper-binding-type) 'lambda-bound)
                       (let ([val (mark-binding-value (lookup-binding mark-list expr))])
                         (and (procedure? val)
                              (not (continuation? val)))))]
                  [(#%top . id-stx)
                   (let ([id (syntax-e (syntax id-stx))])
                     (with-handlers
                         ([exn:variable? (lambda args #f)])
                       (or (and (s:check-pre-defined-var id)
                                (or (procedure? (s:global-lookup id))
                                    (and (s:true-false-printed?)
                                         (or (eq? id 'true)
                                             (eq? id 'false)))))
                           (let ([val (s:global-lookup id)])
                             (and (procedure? val)
                                  (not (continuation? val))
                                  (cond [(closure-table-lookup val (lambda () #f)) =>
                                         (lambda (x)
                                           (eq? var (closure-record-name x)))]
                                        [else #f]))))))]
                  [(#%app . terms)
                   (let ([fun-val (mark-binding-value (lookup-binding mark-list (get-arg-binding 0)))])
                     (and (procedure? fun-val)
                          (procedure-arity-includes? 
                           fun-val
                           (length (cdr (syntax->list (syntax terms)))))
                          (or (and (s:constructor-style-printing?)
                                   (if (s:abbreviate-cons-as-list?)
                                       (or (s:special-function? 'list fun-val)
                                           (and (s:special-function? 'cons fun-val)
                                                (second-arg-is-list? mark-list)))    
                                       (and (s:special-function? 'cons fun-val)
                                            (second-arg-is-list? mark-list))))
                              ;(s:special-function? 'vector fun-val)
                              (and (eq? fun-val void)
                                   (eq? (cdr (syntax->list (syntax terms))) null))
                              (struct-constructor-procedure? fun-val))))])))))
  
  (define (second-arg-is-list? mark-list)
    (let ([arg-val (mark-binding-value (lookup-binding mark-list (get-arg-binding 2)))])
      (list? arg-val)))
    
  (define (in-inserted-else-clause mark-list)
    (and (not (null? mark-list))
         (let ([expr (mark-source (car mark-list))])
           (or (and (z:zodiac? expr)
                    (not (z:if-form? expr))
                    (comes-from-cond? expr))
               (in-inserted-else-clause (cdr mark-list))))))
  
;   ; static-binding-indexer (z:parsed -> integer)
;  
;  (define static-binding-indexer
;    (let* ([name-number-table (make-hash-table)]
;           [binding-number-table (make-hash-table-weak)])
;      (lambda (binding)
;        (cond [(hash-table-get binding-number-table binding (lambda () #f)) =>
;               (lambda (x) x)]
;              [else (let* ([orig-name (z:binding-orig-name binding)]
;                           [old-index (hash-table-get name-number-table orig-name (lambda () -1))]
;                           [new-index (+ old-index 1)])
;                      (hash-table-put! name-number-table orig-name new-index)
;                      (hash-table-put! binding-number-table binding new-index)
;                      new-index)]))))
  
  ; construct-lifted-name (SYNTAX-OBJECT num -> string)
  
  (define (construct-lifted-name binding dynamic-index)
    (string->symbol
     (string-append (symbol->string (syntax-e binding)) "_" 
                    (number->string dynamic-index))))

  ; binding-lifted-name ((listof mark) SYNTAX-OBJECT -> num)
  
  (define (binding-lifted-name mark-list binding)
      (construct-lifted-name binding (mark-binding-value (lookup-binding mark-list (get-lifted-var binding)))))

  ; attach-info : SYNTAX-OBJECT SYNTAX-OBJECT -> SYNTAX-OBJECT
  ; attach-info attaches to a generated piece of syntax the origin & source information of another.
  ; we do this so that macro unwinding can tell what reconstructed syntax came from what original syntax
  (define (attach-info stx expr)
    (let* ([it (syntax-property stx 'user-origin (syntax-property expr 'origin))]
           [it (syntax-property it 'user-source (syntax-source expr))]
           [it (syntax-property it 'user-position (syntax-position expr))])
      it))                                                                                                  
                                                                                                  
                                                                                                  
                                                                ;              ;  ;               
                                                                               ;                  
 ; ;;; ;;    ;;;    ;;;  ; ;;  ;;;       ;   ;  ; ;;  ;   ;   ; ;  ; ;;    ;;; ;  ;  ; ;;    ;; ; 
 ;;  ;;  ;  ;   ;  ;     ;;   ;   ;      ;   ;  ;;  ; ;   ;   ; ;  ;;  ;  ;   ;;  ;  ;;  ;  ;  ;; 
 ;   ;   ;      ;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ; 
 ;   ;   ;   ;;;;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ; 
 ;   ;   ;  ;   ;  ;     ;    ;   ;      ;   ;  ;   ;  ; ; ; ;  ;  ;   ;  ;    ;  ;  ;   ;  ;   ; 
 ;   ;   ;  ;   ;  ;     ;    ;   ;      ;  ;;  ;   ;  ; ; ; ;  ;  ;   ;  ;   ;;  ;  ;   ;  ;  ;; 
 ;   ;   ;   ;;;;;  ;;;  ;     ;;;        ;; ;  ;   ;   ;   ;   ;  ;   ;   ;;; ;  ;  ;   ;   ;; ; 
                                                                                                ; 
                                                                                            ;;;;  
 (define comes-from-define?
    (make-check-raw-first-symbol 'define))

  (define comes-from-define-procedure?
    (make-apply-pred-to-raw
     (lambda (raw) (and (pair? raw)
                        (eq? (car raw) 'define)
                        (pair? (cadr raw))))))
  
  (define comes-from-lambda-defined-procedure?
    (make-apply-pred-to-raw
     (lambda (raw) (and (pair? raw)
                        (eq? (car raw) 'define)
                        (pair? (caddr raw))
                        (eq? (caaddr raw) 'lambda)))))
  
  (define comes-from-define-struct?
    (make-check-raw-first-symbol 'define-struct))
  
  (define comes-from-cond?
    (make-check-raw-first-symbol 'cond))
  
  (define comes-from-lambda?
    (make-check-raw-first-symbol 'lambda))
  
  (define comes-from-case-lambda?
    (make-check-raw-first-symbol 'case-lambda))

  (define comes-from-and?
    (make-check-raw-first-symbol 'and))
  
  (define comes-from-or?
    (make-check-raw-first-symbol 'or))
  
  (define comes-from-local?
    (make-check-raw-first-symbol 'local))  (define (o-form-case-lambda->lambda o-form)
    (cond [(eq? (car o-form) 'lambda)
           o-form]
          [else ; o-form = case-lambda
           (let ([args (caadr o-form)]
                 [body-exps (cdr (cadr o-form))])
             `(lambda ,args ,@body-exps))]))
  
  (define (o-form-lambda->define o-form name)
    (let ([args (cadr o-form)]
          [body-exps (cddr o-form)])
      `(define (,name ,@args) ,@body-exps)))
  
  ; these macro unwinders (and, or) are specific to beginner & intermediate level
  
  (define (rectify-and-clauses and-source expr mark-list lexically-bound-bindings)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-bindings))])
      (if (and (z:if-form? expr) (equal? and-source (z:zodiac-start expr)))
          (cons (rectify-source (z:if-form-test expr))
                (rectify-and-clauses and-source (z:if-form-then expr) mark-list lexically-bound-bindings))
          null)))
  
  (define (rectify-or-clauses or-source expr mark-list lexically-bound-bindings)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-bindings))])
      (if (and (z:if-form? expr) (equal? or-source (z:zodiac-start expr)))
          (cons (rectify-source (z:if-form-test expr))
                (rectify-or-clauses or-source (z:if-form-else expr) mark-list lexically-bound-bindings))
          null)))
  
  (define (rectify-cond-clauses cond-source expr mark-list lexically-bound-bindings)
    (let ([rectify-source (lambda (expr) (rectify-source-expr expr mark-list lexically-bound-bindings))])
      (if (equal? cond-source (z:zodiac-start expr))
          (if (z:if-form? expr)
              (cons (list (rectify-source (z:if-form-test expr))
                          (rectify-source (z:if-form-then expr)))
                    (rectify-cond-clauses cond-source (z:if-form-else expr) mark-list lexically-bound-bindings))
              null)
          `((else ,(rectify-source expr))))))
  
  (define (rectify-local raw name-sets right-sides body)
    (let ([define-clauses (cadr raw)])
      `(local
           ,(map 
             (lambda (clause name-set right-side)
               (case (car clause)
                 ((define-struct) clause)
                 ((define)
                  (cond [(pair? (cadr clause)) 
                         (unless (eq? (car right-side) 'lambda)
                           (error 'rectify-local "define-proc form in local doesn't match reconstructed rhs."))
                         (o-form-lambda->define right-side (car name-set))]
                        [else
                         `(define ,(car name-set) ,right-side)]))
                 ((define-values)
                  `(define-values ,name-set ,right-side))))
             define-clauses name-sets right-sides)
         ,body)))
  
;  (equal? (rectify-local '(local ((define (ident x) x)
;                                  (define another-ident (lambda (x) x))
;                                  (define a 6)
;                                  (define-values (m n) (values 45 2))
;                                  (define-struct p (x y)))
;                            (ident a))
;                         '((ident) (another-ident) (a) (m n) (p))
;                         '((lambda (x) x)
;                           (lambda (x) x)
;                           6
;                           (values 45 2)
;                           (struct a b c))
;                         '(ident a))
;          '(local ((define (ident x) x)
;                                  (define another-ident (lambda (x) x))
;                                  (define a 6)
;                                  (define-values (m n) (values 45 2))
;                                  (define-struct p (x y)))
;                            (ident a)))

                                                                                                               
 ; ;;  ;;;    ;;;   ;;;   ; ;;           ;;;   ;;;   ;   ;  ; ;;  ;;;   ;;;           ;;;  ;    ;  ; ;;;   ; ;;
 ;;   ;   ;  ;     ;   ;  ;;  ;         ;     ;   ;  ;   ;  ;;   ;     ;   ;         ;   ;  ;  ;   ;;   ;  ;;  
 ;    ;   ;  ;     ;   ;  ;   ;         ;     ;   ;  ;   ;  ;    ;     ;   ;         ;   ;   ;;    ;    ;  ;   
 ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;   ;;   ;   ;  ;   ;  ;    ;     ;;;;;  ;;;;;  ;;;;;   ;;    ;    ;  ;   
 ;    ;      ;     ;   ;  ;   ;            ;  ;   ;  ;   ;  ;    ;     ;             ;       ;;    ;    ;  ;   
 ;    ;      ;     ;   ;  ;   ;            ;  ;   ;  ;  ;;  ;    ;     ;             ;      ;  ;   ;;   ;  ;   
 ;     ;;;;   ;;;   ;;;   ;   ;         ;;;    ;;;    ;; ;  ;     ;;;   ;;;;          ;;;; ;    ;  ; ;;;   ;   
                                                                                                   ;           
                                                                                                   ;           
                                                                                                               

  ; recon-source-expr (SYNTAX-OBJECT (list-of Mark) BINDING-SET -> SYNTAX-OBJECT)
  
  ; recon-source-expr produces the reconstructed version of a given source epxression, using the binding
  ; information contained in the binding-list.  This happens during reconstruction whenever we come upon
  ; expressions that we haven't yet evaluated. 
  
  ; NB: the variable 'lexically-bound-bindings' contains a list of bindings which occur INSIDE the expression
  ; being evaluated, and hence do NOT yet have values.
  
  (define (recon-source-expr expr mark-list)
      (letrec
        ([inner
          (checked-lambda ((expr SYNTAX-OBJECT) (mark-list MARK-LIST) (lexically-bound-bindings BINDING-SET))
            (let ([recur (lambda (expr) (inner expr mark-list lexically-bound-bindings))]
                  [let-recur (lambda (expr bindings)
                               (inner expr mark-list (append bindings lexically-bound-bindings)))]
                  
                  [recon-basic
                   (lambda ()
                     (with-syntax ([(label . bodies) expr])
                       (d->so `((syntax label) ,@(map recur (syntax->list (syntax bodies)))))))]
                  [recon-let/rec
                   (lambda ()
                     (with-syntax ([(label  ((vars val) ...) body) expr])
                       (let* ([bindings (map syntax->list (syntax->list (syntax (vars ...))))]
                              [binding-list (multi-append bindings)]
                              [right-sides (map recur (syntax->list (syntax (val ...))))]
                              [recon-body (let-recur (syntax body) binding-list)])
                         (with-syntax ([(recon-val ...) right-sides]
                                       [recon-body recon-body])
                           (syntax (label ((vars recon-val) ...) recon-body))))))]
                  [recon-lambda-clause
                   (lambda (clause)
                     (with-syntax ([(args . bodies-stx) clause])
                       (let* ([arglist (ilist-flatten (syntax->ilist args))]
                              [bodies (map (lambda (body) (let-recur body arglist))
                                           (syntax->list (syntax bodies-stx)))])
                         (cons (syntax args) bodies))))]
                  [recon (kernel:kernel-syntax-case expr #f
                                                    
                     ; lambda
                     [(lambda . clause-stx)
                      (let* ([clause (recon-lambda-clause (syntax clause-stx))])
                        (d->so `(lambda ,@clause)))]
                     
                     ; case-lambda
                     [(case-lambda . clauses-stx)
                      (let* ([clauses (map recon-lambda-clause (syntax->list (syntax clauses-stx)))])
                        (d->so `(case-lambda ,@clauses)))]
                     
                     ; if, begin, begin0
                     [(if test then else) (recon-basic)]
                     [(if test then) (recon-basic)]
                     [(begin . bodies) (recon-basic)]
                     [(begin0 . bodies) (recon-basic)]
                     
                     ; let-values, letrec-values
                     [(let-values . rest) (recon-let/rec)]
                     [(letrec-values . rest) (recon-let/rec)]
                     
                     ; set! : set! doesn't fit into this scheme. It would be a mistake to allow it to proceed.
                     
                     ; quote 
                     [(quote body) (recon-value (syntax-e body))]
                     
                     ; quote-syntax : like set!, the current stepper cannot handle quote-syntax
                     
                     ; with-continuation-mark
                     [(with-continuation-mark . rest) (recon-basic)]
                     
                     ; application
                     [(#%app . terms) (d->so (map recur (syntax->list (syntax terms))))]
                     
                     ; #%datum
                     [(#%datum . datum) (recon-value (syntax-e body))]
                     
                     ; varref                        
                     [var-stx
                      (identifier? expr)
                      (let* ([var (syntax var)])
                        (cond [(eq? (identifier-binding var) 'lexical)
                               ; has this varref's binding not been evaluated yet?
                               (if (ormap (lambda (binding)
                                            (bound-identifier=? binding var))
                                          lexically-bound-bindings)
                                   var
                                   (case (syntax-property var 'stepper-binding-type)
                                     ((lambda-bound) 
                                      (rectify-value (mark-binding-value (lookup-binding mark-list binding))))
                                     ((let-bound)
                                      (d->so (binding-lifted-name mark-list binding)))
                                     (else
                                      (error 'recon-source-expr "no 'stepper-binding-type property found"))))]
                              [else ; top-level-varref
                               var]))]
                     
                     [else
                      (error 'recon-source "no matching clause for syntax: ~a" expr)])])
              (attach-info recon expr)))])
        (inner expr mark-list null)))
 
  
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                        ;                         ; 
                                       ;                     ;                                          ;         ;               ; 
 ; ;;  ;;;    ;;;   ;;;   ; ;;    ;;; ;;;; ; ;; ;   ;   ;;; ;;;;         ;;;   ;;;   ; ;;; ;;   ; ;;;   ;   ;;;  ;;;;  ;;;    ;;; ; 
 ;;   ;   ;  ;     ;   ;  ;;  ;  ;     ;   ;;   ;   ;  ;     ;          ;     ;   ;  ;;  ;;  ;  ;;   ;  ;  ;   ;  ;   ;   ;  ;   ;; 
 ;    ;   ;  ;     ;   ;  ;   ;  ;     ;   ;    ;   ;  ;     ;          ;     ;   ;  ;   ;   ;  ;    ;  ;  ;   ;  ;   ;   ;  ;    ; 
 ;    ;;;;;  ;     ;   ;  ;   ;   ;;   ;   ;    ;   ;  ;     ;   ;;;;;  ;     ;   ;  ;   ;   ;  ;    ;  ;  ;;;;;  ;   ;;;;;  ;    ; 
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;   ;  ;     ;          ;     ;   ;  ;   ;   ;  ;    ;  ;  ;      ;   ;      ;    ; 
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;  ;;  ;     ;          ;     ;   ;  ;   ;   ;  ;;   ;  ;  ;      ;   ;      ;   ;; 
 ;     ;;;;   ;;;   ;;;   ;   ;  ;;;    ;; ;     ;; ;   ;;;   ;;         ;;;   ;;;   ;   ;   ;  ; ;;;   ;   ;;;;   ;;  ;;;;   ;;; ; 
                                                                                                ;                                   
                                                                                                ;                                   
                                                                                                                                    

  ; reconstruct-completed : reconstructs a completed expression or definition.  This now
  ; relies upon the s:global-lookup procedure to find values in the user-namespace.
  ; I'm not yet sure whether or not 'vars' must be supplied or whether they can be derived
  ; from the expression itself.
  
  (define (reconstruct-completed expr value)    
      (cond [(z:define-values-form? expr)
             (if (comes-from-define-struct? expr)
                 (utils:read->raw (expr-read expr))
                 (let* ([vars (map z:varref-var (z:define-values-form-vars expr))]
                        [values (map s:global-lookup vars)]
                        [rectified-vars (map rectify-value values)])
                   (cond [(comes-from-define-procedure? expr)
                          (let* ([mark (closure-record-mark  (closure-table-lookup (car values)))]
                                 [rectified (recon-source-expr (mark-source mark) (list mark))])
                            (o-form-lambda->define (o-form-case-lambda->lambda rectified)
                                                   (car vars)))]
                         [(comes-from-lambda-defined-procedure? expr)
                          (let* ([mark (closure-record-mark (closure-table-lookup (car values)))]
                                 [rectified (recon-source-expr (mark-source mark) (list mark))])
                            `(define ,(car vars) ,(o-form-case-lambda->lambda rectified)))]
                         [(comes-from-define? expr)
                          `(define ,(car vars) ,(car rectified-vars))]
                         [else
                          `(define-values ,vars
                             ,(if (= (length values) 1)
                                  (car rectified-vars)
                                  `(values ,@rectified-vars)))])))]
            [(z:begin-form? expr) ; hack for xml stuff
             (utils:read->raw (expr-read expr))]
            [else
             (rectify-value value)]))
  
  
  ; reconstruct-lifted ((listof symbol) sexp -> sexp)
  ; reconstruct-lifted really should take into account the original source expression. Local may
  ; screw me up. We'll cross that bridge when we get to it.
  
  (define (reconstruct-lifted names sexp)
    (case (length names)
      ((0) `(define-values () ,sexp))
      ((1) (if (and (pair? sexp)
                    (eq? (car sexp) 'lambda))
               (o-form-lambda->define sexp (car names))
               `(define ,(car names) ,sexp)))
      (else `(define-values ,names ,sexp))))
       
  (define (reconstruct-lifted-val name val)
    (let ([rectified-val (let-rhs-rectify-value val)])
      (if (and (procedure? val)
               (pair? rectified-val)
               (eq? (car rectified-val) 'lambda))
          (o-form-lambda->define rectified-val name)
          `(define ,name ,rectified-val))))
  
  (define (so-far-only so-far) (values null null so-far))
 

                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
                                       ;                     ;                                               ;  
 ; ;;  ;;;    ;;;   ;;;   ; ;;    ;;; ;;;; ; ;; ;   ;   ;;; ;;;;         ;;;  ;   ;  ; ;; ; ;;  ;;;   ; ;;  ;;;;
 ;;   ;   ;  ;     ;   ;  ;;  ;  ;     ;   ;;   ;   ;  ;     ;          ;     ;   ;  ;;   ;;   ;   ;  ;;  ;  ;  
 ;    ;   ;  ;     ;   ;  ;   ;  ;     ;   ;    ;   ;  ;     ;          ;     ;   ;  ;    ;    ;   ;  ;   ;  ;  
 ;    ;;;;;  ;     ;   ;  ;   ;   ;;   ;   ;    ;   ;  ;     ;   ;;;;;  ;     ;   ;  ;    ;    ;;;;;  ;   ;  ;  
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;   ;  ;     ;          ;     ;   ;  ;    ;    ;      ;   ;  ;  
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;  ;;  ;     ;          ;     ;  ;;  ;    ;    ;      ;   ;  ;  
 ;     ;;;;   ;;;   ;;;   ;   ;  ;;;    ;; ;     ;; ;   ;;;   ;;         ;;;   ;; ;  ;    ;     ;;;;  ;   ;   ;;
                                                                                                                
                                                                                                                
                                                                                                                
  
  ; reconstruct-current : takes a parsed expression, a list of marks, the kind of break, and
  ; any values that may have been returned at the break point. It produces a list containing the
  ; reconstructed sexp, and the (contained) sexp which is the redex.  If the redex is a heap value
  ; (and can thus be distinguished from syntactically identical occurrences of that value using
  ; eq?), it is embedded directly in the sexp. Otherwise, its place in the sexp is taken by the 
  ; highlight-placeholder, which is replaced by the highlighted redex in the construction of the 
  ; text%
  
  reconstruct-current : SYNTAX-OBJECT (list-of mark) symbol (list-of value) -> (list SYNTAX-OBJECT SYNTAX-OBJECT)

  (define (reconstruct-current expr mark-list break-kind returned-value-list)
    
    (local
        ((define (rectify-top-level expr so-far)
           (if (z:define-values-form? expr)
               (let ([vars (z:define-values-form-vars expr)]
                     [val (z:define-values-form-val expr)])
                 (cond [(comes-from-define-struct? expr)
                        (let* ([struct-expr val]
                               [super-expr (z:struct-form-super struct-expr)]
                               [raw-type (utils:read->raw (z:struct-form-type struct-expr))]
                               [raw-fields (map utils:read->raw (z:struct-form-fields struct-expr))])
                          `(define-struct
                            ,(if super-expr
                                 (list raw-type so-far)
                                 raw-type)
                            ,raw-fields))]
                       [(or (comes-from-define-procedure? expr)
                            (and (comes-from-define? expr)
                                 (pair? so-far)
                                 (eq? (car so-far) 'lambda)))
                        (let* ([proc-name (z:varref-var
                                           (car (z:define-values-form-vars expr)))]
                               [o-form-proc so-far])
                          (o-form-lambda->define o-form-proc proc-name))]
                                              
                       [(comes-from-define? expr)
                        `(define 
                           ,(z:varref-var (car vars))
                           ,so-far)]
                       
                       [else
                        `(define-values 
                           ,(map utils:read->raw vars)
                           ,(recon-source-top-marks val mark-list))]))
               so-far))

                                                ;                           
                                                                    
         ; ;;  ;;;    ;;;   ;;;   ; ;;          ;  ; ;;   ; ;;    ;;;   ; ;;
         ;;   ;   ;  ;     ;   ;  ;;  ;         ;  ;;  ;  ;;  ;  ;   ;  ;;  
         ;    ;   ;  ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;   ;  ;   
         ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;  ;  ;   ;  ;   ;  ;;;;;  ;   
         ;    ;      ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;      ;   
         ;    ;      ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;      ;   
         ;     ;;;;   ;;;   ;;;   ;   ;         ;  ;   ;  ;   ;   ;;;;  ;   
                                                                       

         ; recon-inner ((listof mark) SYNTAX-OBJECT -> (listof SYNTAX-OBJECT) (listof SYNTAX-OBJECT) SYNTAX-OBJECT)
         
         (define (recon-inner mark-list so-far)
           (let* ([recon-source-current-marks 
                   (lambda (expr)
                     (recon-source-expr expr mark-list))]
                  [top-mark (car mark-list)]
                  [expr (mark-source top-mark)]
                  [recon-let
                   (lambda ()
                     (with-syntax ([(label ((vars val) ...) . bodies)])
                       (let*-2vals ([binding-sets (map syntax->list (syntax->list (syntax (vars ...))))]
                                    [binding-list (multi-append binding-sets)]
                                    [dummy-var-list (if letrec?
                                                        binding-list
                                                        (build-list (length binding-list) get-arg-binding))]
                                    [rhs-vals (map (lambda (arg-binding) 
                                                     (mark-binding-value (lookup-binding mark-list arg-binding)))
                                                   dummy-var-list)]
                                    [rhs-val-sets (reshape-list rhs-vals binding-sets)]
                                    [rhs-lifted-name-sets
                                     (map (lambda (binding-set)
                                            (map (lambda (binding)
                                                   (binding-lifted-name mark-list binding))
                                                 binding-set))
                                          binding-sets)]
                                    [zipped (zip rhs-val-sets rhs-sources rhs-lifted-name-sets)]
                                    [num-defns-done (mark-binding-value (lookup-binding mark-list let-counter))]
                                    [(done-defs not-done-defs)
                                     (n-split-list num-defns-done zipped)]
                                    [before-defs
                                     (multi-append
                                      (map
                                       (lambda (info)
                                         (let* ([rhs-val-set (car info)]
                                                [rhs-lifted-name-set (caddr info)])
                                           (map reconstruct-lifted-val rhs-lifted-name-set rhs-val-set)))
                                       done-defs))]
                                    [reconstruct-remaining-def
                                     (lambda (rhs-lifted-name-set rhs-source raw-local-source)
                                       (let ([rhs-source (cadr info)]
                                             [rhs-lifted-name-set (caddr info)])
                                         (reconstruct-lifted rhs-lifted-name-set
                                                             (recon-source-expr-current-marks rhs-source))))]
                                    [after-defs
                                     (if (pair? not-done-defs)
                                         (if (eq? so-far nothing-so-far)
                                             (map reconstruct-remaining-def not-done-defs)
                                             (cons (reconstruct-lifted (car rhs-lifted-name-sets) so-far)
                                                   (map reconstruct-remaining-def (cdr not-done-defs))))
                                         null)]
                                    [rectified-body (recon-source-expr body mark-list)])
                         (values before-defs after-defs rectified-body))))])
             (kernel:kernel-syntax-case expr #f 
               ; variable references
               [id
                (identifier? id)
                (so-far-only
                 (if (eq? so-far nothing-so-far)
                     (recon-source-current-marks expr)
                     (error 'recon-inner "variable reference given as context: ~a" expr)))]
               
               ; applications
               [(#%app . terms)
                (so-far-only
                 (let* ([sub-exprs (syntax->list (syntax terms))]
                        [arg-temps (build-list (length sub-exprs) get-arg-binding)]
                        [arg-vals (map (lambda (arg-temp) 
                                         (mark-binding-value (lookup-binding mark-list arg-temp)))
                                       arg-temps)])
                   (case (mark-label (car mark-list))
                     ((not-yet-called)
                      (let*-2vals ([(evaluated unevaluated) (split-list (lambda (x) (eq? x *unevaluated*))
                                                                        (zip sub-exprs arg-vals))]
                                   [rectified-evaluated (map rectify-value evaluated)])
                        (d->so
                         (if (null? unevaluated)
                             rectified-evaluated
                             `(,@rectified-evaluated
                               so-far 
                               ,@(map recon-source-current-marks (cdr unevaluated)))))))
                     ((called)
                      (d->so
                       (if (eq? so-far nothing-so-far)
                           `(...) ; in unannotated code
                           `(... ,so-far ...))))
                     (else
                      (internal-error expr "bad label in application mark")))))]
               
               ; define-struct 
;               
;               [(z:struct-form? expr)
;                (so-far-only
;                 (if (comes-from-define-struct? expr)
;                     so-far
;                     (let ([super-expr (z:struct-form-super expr)]
;                           [raw-type (utils:read->raw (z:struct-form-type expr))]
;                           [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
;                       (if super-expr
;                           `(struct (,raw-type ,so-far)
;                                    ,raw-fields)
;                           `(struct ,raw-type ,raw-fields)))))]
               
               ; if
               [(if test then else)
                (so-far-only
                 (let ([test-exp (if (eq? so-far nothing-so-far)
                                     (recon-source-current-marks (syntax test))
                                     so-far)])
                   (d->so `(if ,test-exp 
                               ,(recon-source-current-marks (z:if-form-then expr))
                               ,(recon-source-current-marks (z:if-form-else expr))))))]
               
               ; quote : there is no break on a quote.
               
               ; begin, begin0 : may not occur directly (or indirectly?) except in advanced
               
               ; let-values
               
               [(let-values . rest) (recon-let)]
               
               [(letrec-values . rest) (recon-let)]
               
               ; define-values : define's don't get marks, so they can't occur here
               
               ; lambda : there is no break on a lambda
               
               [else
                (internal-error
                 expr
                 (format "stepper:reconstruct: unknown object to reconstruct, ~a~n" expr))])))
         
         
         (define redex #f)
         
         ; the main recursive reconstruction loop is in recon:
         (define (recon defs so-far mark-list first)
           (if (null? mark-list)
               (append defs
                       (list (recon-top-level expr so-far)))
               (let-values ([(before after reconstructed) (recon-inner mark-list so-far)])
                 (recon
                  (append before defs after)
                  (if first
                      (begin
                        (set! redex reconstructed)
                        highlight-placeholder)
                      reconstructed)
                  (cdr mark-list)
                  #f))))
         
         (define (rectify-let-values-step)
           (let*-values ([(redex) (recon-source-expr (mark-source (car mark-list)) mark-list)]
                         [(before-step) (recon null highlight-placeholder (cdr mark-list) #f)]
                         [(r-before r-after reduct) (recon-inner mark-list nothing-so-far)]
                         [(new-defs) (append r-before r-after)]
                         [(after-step) (recon (build-list (length new-defs) 
                                                          (lambda (x) highlight-placeholder))
                                              highlight-placeholder
                                              (cdr mark-list) 
                                              #f)])
             (list before-step (list redex)
                   after-step (append new-defs (list reduct)))))
           
         (define answer
           (map syntax-object->datum
                (case break-kind
                  ((result-break)
                   (let* ([innermost (if (null? returned-value-list) ; is it an expr -> expr reduction?
                                         (recon-source-expr (mark-source (car mark-list)) mark-list)
                                         (recon-value (car returned-value-list)))]
                          [current-defs (recon null highlight-placeholder (cdr mark-list) #f)])
                     (list current-defs (list innermost))))
                  ((normal-break)
                   (let ([current-defs (recon null nothing-so-far mark-list #t)])
                     (list current-defs (list redex))))
                  ((double-break)
                   (rectify-let-values-step))
                  ((late-let-break)
                   (let-values ([(before after junk) (recon-inner mark-list nothing-so-far)])
                     (unless (null? after)
                       (error 'answer "non-empty 'after' defs in late-let-break"))
                     before))
                  (else
                   (error 'reconstruct-current-def "unknown break kind: " break-kind)))))

         )
      
      answer)))
