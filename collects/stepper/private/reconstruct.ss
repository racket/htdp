; general assertions about reconstruction:
; a varref can only occur at the top of a mark-list
; a varref at the top of the mark-list must either be a top-level-variable
;  or have a value in some mark somewhere (or both).

(module reconstruct mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "list.ss")
           (lib "etc.ss")
           "marks.ss"
           (prefix model-settings: "model-settings.ss")
           "shared.ss"
           "highlight-placeholder.ss"
           "my-macros.ss"
           (lib "specs.ss" "framework"))

  (provide
   reconstruct-completed
   reconstruct-current ; : syntax (list-of mark) symbol (list-of value) -> (list (listof sexp) (listof sexp))
   final-mark-list?
   skip-step?)
  
  (make-contract-checker STRING string?)

  (make-contract-checker MARK-LIST 
                         (lambda (arg) 
                           (andmap procedure? arg))) 
  
  (define the-undefined-value (letrec ([x x]) x))
  
  (define nothing-so-far (gensym "nothing-so-far-"))
  
  (define highlight-placeholder-stx (datum->syntax-object #'here highlight-placeholder))

  ; the let-glump is a structure that contains the reconstruct-time data about
  ; a let-binding; that is, the names on the left-hand-side, the expression on
  ; the right-hand side, and the values computed.
  
  (define-struct let-glump (name-set exp val-set))
                                                                                                                

  ; loloval? : TST -> boolean
  ; loloval? returns true if given a list of lists of values
  
  (define (loloval? val)
    (and (list? val)
         (andmap list? val)))
                        

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
  ; (test (2vals '(a b c) '(d e f)) n-split-list 3 '(a b c d e f))

  ; multi-append : (('a list) list) -> ('a list)
  ; applies append to a bunch of lists.  Would be trivial, except I'm trying to avoid
  ; using 'apply'.
  
  (define (multi-append lol)
    (foldl (lambda (a b) (append b a)) null lol))
  
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
                                                                     
                                                                     
                                                     ;               
                                                     ;               
 ; ;;  ;;;    ;;;   ;;;   ; ;;         ;   ;   ;;;   ;  ;   ;   ;;;  
 ;;   ;   ;  ;     ;   ;  ;;  ;        ;   ;  ;   ;  ;  ;   ;  ;   ; 
 ;    ;   ;  ;     ;   ;  ;   ;         ; ;       ;  ;  ;   ;  ;   ; 
 ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;  ; ;    ;;;;  ;  ;   ;  ;;;;; 
 ;    ;      ;     ;   ;  ;   ;         ; ;   ;   ;  ;  ;   ;  ;     
 ;    ;      ;     ;   ;  ;   ;         ;;    ;   ;  ;  ;  ;;  ;     
 ;     ;;;;   ;;;   ;;;   ;   ;          ;     ;;;;; ;   ;; ;   ;;;; 
                                                                     
                                                                     
                                                                     
  ; recon-value : value -> syntax-object
  ; recon-value print-converts a value.  If the value is a closure, recon-value
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
                  (recon-source-expr (mark-source mark) (list mark) null))])]
        [else
         (d->so (model-settings:print-convert val))])))
  
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
                                                                                         
                                                                                         
       ;      ;                                                                     ;;;  
       ;                                                          ;                    ; 
  ;;;  ;   ;  ;  ; ;;;         ;    ; ;    ; ;    ;          ;;; ;;;;  ;;;   ; ;;;     ; 
 ;     ;  ;   ;  ;;   ;         ;  ;   ;  ;   ;  ;          ;     ;   ;   ;  ;;   ;    ; 
 ;     ; ;    ;  ;    ;          ;;     ;;     ;;           ;     ;   ;   ;  ;    ;   ;  
  ;;   ;;     ;  ;    ;  ;;;;;   ;;     ;;     ;;    ;;;;;   ;;   ;   ;;;;;  ;    ;  ;   
    ;  ; ;    ;  ;    ;          ;;     ;;     ;;              ;  ;   ;      ;    ;  ;   
    ;  ;  ;   ;  ;;   ;         ;  ;   ;  ;   ;  ;             ;  ;   ;      ;;   ;      
 ;;;   ;   ;  ;  ; ;;;         ;    ; ;    ; ;    ;         ;;;    ;;  ;;;;  ; ;;;   ;   
                 ;                                                           ;           
                 ;                                                           ;           
                                                                                         
  (define skip-step?
    (contract
     (-> break-kind? mark-list? boolean?)
     (lambda (break-kind mark-list)
       (case break-kind
         ((result-value-break result-exp-break)
          #f)
         ((normal-break)
          (skip-redex-step? mark-list))
         ((double-break late-let-break)
          (error 'answer "lifting turned off"))))
     'reconstruct-module
     'caller))
  
  (define (skip-redex-step? mark-list)
    (and (pair? mark-list)
         (let ([expr (mark-source (car mark-list))])
           (or (kernel:kernel-syntax-case expr #f
                  [id
                   (identifier? expr)
                   (or (and (model-settings:true-false-printed?) ; if our language prints #t as true, then ...
                            (cons? (identifier-binding (syntax id)))      ; for module-bound identifiers,
                            (or (eq? (syntax-e (syntax id)) 'true)        ; don't halt for true or false
                                (eq? (syntax-e (syntax id)) 'false)))
                       (eq? (syntax-property expr 'stepper-binding-type) 'lambda-bound) ; don't halt for lambda-bound vars
                       (let ([val (mark-binding-value (lookup-binding mark-list expr))]) 
                         (and (procedure? val)
                              (not (continuation? val)))))] ; don't halt for varrefs bound to non-continuation procedures
                  [(#%top . id-stx)
                   (let ([id (syntax id-stx)])
                     (with-handlers
                         ([exn:variable? (lambda args #f)]) ; DO halt for unbound top-level varrefs
                       (let ([val (model-settings:global-lookup (syntax-e id))])
                         (or (and (procedure? val)                     ; don't halt for top-level procedure refs ...
                                  (eq? (syntax-e id) (object-name val)) ; with the right inferred name
                                  
                                  ; Do we need this stuff for lifted names? :
                                  
                                  (or (not (closure-table-lookup val (lambda () #f))) ; that are primitives, or ...
                                      (and (not (continuation? val))
                                           (cond [(closure-table-lookup val (lambda () #f)) => ; are user fns with the right (original) name
                                                  (lambda (x)
                                                    (eq? id (closure-record-name x)))] ; has wrong name
                                                 [else #f]))))))))]
                  [(#%app . terms)
                   ; don't halt for proper applications of constructors
                   (let ([fun-val (mark-binding-value (lookup-binding mark-list (get-arg-var 0)))])
                     (and (procedure? fun-val)
                          (procedure-arity-includes? 
                           fun-val
                           (length (cdr (syntax->list (syntax terms)))))
                          (or (and (model-settings:constructor-style-printing?)
                                   (if (model-settings:abbreviate-cons-as-list?)
                                       (or (eq? fun-val (model-settings:global-lookup 'list))
                                           (and (eq? fun-val (model-settings:global-lookup 'cons))
                                                (second-arg-is-list? mark-list)))    
                                       (and (eq? fun-val (model-settings:global-lookup 'cons))
                                            (second-arg-is-list? mark-list))))
                              ;(model-settings:special-function? 'vector fun-val)
                              (and (eq? fun-val void)
                                   (eq? (cdr (syntax->list (syntax terms))) null))
                              (struct-constructor-procedure? fun-val))))]
                  [else
                   #f])))))
  
  (define (second-arg-is-list? mark-list)
    (let ([arg-val (mark-binding-value (lookup-binding mark-list (get-arg-var 2)))])
      (list? arg-val)))
    
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

  (define (unwind stx-list highlights)
    (local
        ((define highlight-queue-src (make-queue))
         (define highlight-queue-dest (make-queue))
         
         (define (recur-on-pieces stx)
           (if (pair? (syntax-e stx))
               (datum->syntax-object stx (syntax-pair-map (syntax-e stx) inner) stx stx)
               stx))
         
         (define (inner stx)
           (if (eq? stx highlight-placeholder-stx)
               (begin (queue-push highlight-queue-dest (inner (queue-pop highlight-queue-src)))
                      highlight-placeholder-stx)
               (if (syntax-property stx 'user-stepper-hint)
                   (case (syntax-property stx 'user-stepper-hint)
                     ((comes-from-cond) (unwind-cond stx 
                                                     (syntax-property stx 'user-source)
                                                     (syntax-property stx 'user-position)))
                     ((comes-from-and) (unwind-and/or stx
                                                      (syntax-property stx 'user-source)
                                                      (syntax-property stx 'user-position)
                                                      'and))
                     ((comes-from-or) (unwind-and/or stx
                                                     (syntax-property stx 'user-source)
                                                     (syntax-property stx 'user-position)
                                                     'or))
                     (else (recur-on-pieces stx)))
                   (recur-on-pieces stx))))
         
         (define (unwind-cond stx user-source user-position)
           (if (eq? stx highlight-placeholder-stx)
               (begin (queue-push highlight-queue-dest (unwind-cond (queue-pop highlight-queue-src) user-source user-position))
                      highlight-placeholder-stx)
               (with-syntax ([clauses
                              (let loop ([stx stx])
                                (if (or (and (eq? user-source (syntax-property stx 'user-source))
                                             (eq? user-position (syntax-property stx 'user-position)))
                                        (syntax-property stx 'user-stepper-else))
                                    (syntax-case stx (if begin)
                                      [(if test result else)
                                       (cons (inner (syntax (test result)))
                                             (loop (syntax else)))]
                                      [else-stx
                                       ; source or synthesized else?
                                       (if (eq? 'inserted-else (syntax-property (syntax else-stx) 'user-stepper-hint))
                                           null
                                           (cons (inner (syntax (else else-stx)))
                                                 null))])
                                    (error 'unwind-cond "unexpected source for cond element ~a\n" (syntax-object->datum stx))))])
                 (syntax (cond . clauses)))))
         
         (define (unwind-and/or stx user-source user-position label)
           (if (eq? stx highlight-placeholder-stx)
               (begin (queue-push highlight-queue-dest (unwind-and/or (queue-pop highlight-queue-src) user-source user-position))
                      highlight-placeholder-stx)
               (with-syntax ([label (datum->syntax-object #f label)]
                             [clauses
                              (let loop ([stx stx])
                                (if (and (eq? user-source (syntax-property stx 'user-source))
                                         (eq? user-position (syntax-property stx 'user-position)))
                                    (syntax-case stx (if let-values)
                                      [(let-values (((part-0) test-stx)) (if part-1 part-2 part-3))
                                       (cons (inner (syntax test-stx))
                                             (case label
                                               ((and)
                                                (loop (syntax part-2)))
                                               ((or)
                                                (loop (syntax part-3)))
                                               (else
                                                (error 'unwind-and/or "unknown label ~a" label))))]
                                      [(if part-1 part-2 part-3)
                                       (cons (inner (syntax part-1))
                                             (case label
                                               ((and)
                                                (loop (syntax part-2)))
                                               ((or)
                                                (loop (syntax part-3)))
                                               (else
                                                (error 'unwind-and/or "unknown label ~a" label))))]
                                      [else (error 'unwind-and/or "syntax: ~a does not match and/or patterns" (syntax-object->datum stx))])
                                    (list (inner stx))))])
                 (syntax (label . clauses))))))
      
      (for-each (lambda (x) (queue-push highlight-queue-src x)) highlights)
      (let* ([main (map inner stx-list)]
             [new-highlights (build-list (queue-length highlight-queue-dest) (lambda (x) (queue-pop highlight-queue-dest)))])
        (list main new-highlights))))
  
  ; attach-info : SYNTAX-OBJECT SYNTAX-OBJECT -> SYNTAX-OBJECT
  ; attach-info attaches to a generated piece of syntax the origin & source information of another.
  ; we do this so that macro unwinding can tell what reconstructed syntax came from what original syntax
  (define (attach-info stx expr)
    (let* ([it (syntax-property stx 'user-origin (syntax-property expr 'origin))]
           [it (syntax-property it 'user-stepper-hint (syntax-property expr 'stepper-hint))]
           [it (syntax-property it 'user-stepper-else (syntax-property expr 'stepper-else))]
           [it (syntax-property it 'user-source (syntax-source expr))]
           [it (syntax-property it 'user-position (syntax-position expr))])
      it))                                                                                                  
                                                                                                  

                                                                                                               
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

  (define recon-source-expr 
    (contract
     (-> syntax? mark-list? binding-set? syntax?)
     (lambda (expr mark-list lexically-bound-bindings)
      (if (syntax-property expr 'stepper-skipto)
               (skipto-reconstruct
                (syntax-property expr 'stepper-skipto)
                expr
                (lambda (stx)
                  (recon-source-expr stx mark-list lexically-bound-bindings)))
               (let* ([recur (lambda (expr) (recon-source-expr expr mark-list lexically-bound-bindings))]
                      [let-recur (lambda (expr bindings)
                                   (recon-source-expr expr mark-list (append bindings lexically-bound-bindings)))]
                      
                      [recon-basic
                       (lambda ()
                         (with-syntax ([(label . bodies) expr])
                           (d->so `(,(syntax label) ,@(map recur (syntax->list (syntax bodies)))))))]
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
                           (let* ([arglist (ilist-flatten (syntax->ilist (syntax args)))]
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
                               [(quote body) (recon-value (syntax-e (syntax body)))]
                               
                               ; quote-syntax : like set!, the current stepper cannot handle quote-syntax
                               
                               ; with-continuation-mark
                               [(with-continuation-mark . rest) (recon-basic)]
                               
                               ; application
                               [(#%app . terms) 
                                (d->so (map recur (syntax->list (syntax terms))))]
                               
                               ; #%datum
                               [(#%datum . datum) (recon-value (syntax-e (syntax datum)))]
                               
                               ; varref                        
                               [var-stx
                                (identifier? expr)
                                (let* ([var (syntax var-stx)])
                                  (cond [(eq? (identifier-binding var) 'lexical)
                                         ; has this varref's binding not been evaluated yet?
                                         (if (ormap (lambda (binding)
                                                      (bound-identifier=? binding var))
                                                    lexically-bound-bindings)
                                             var
                                             (case (syntax-property var 'stepper-binding-type)
                                               ((lambda-bound) 
                                                (recon-value (mark-binding-value (lookup-binding mark-list var))))
                                               ((let-bound)
                                                ; for the moment, let-bound vars occur only in and/or :
                                                (recon-value (mark-binding-value (lookup-binding mark-list var))))
                                                ; (d->so (binding-lifted-name mark-list var)))
                                               ((top-level) var)
                                               ((stepper-temp)
                                                (error 'recon-source-expr "stepper-temp showed up in source?!?"))
                                               (else
                                                (error 'recon-source-expr "unknown 'stepper-binding-type property: ~a" 
                                                       (syntax-property var 'stepper-binding-type)))))]
                                        [else ; top-level-varref
                                         var]))]
                               [(#%top . var)
                                (syntax var)]
                               
                               [else
                                (error 'recon-source "no matching clause for syntax: ~a" expr)])])
                 (attach-info recon expr))))
     'recon-source-expr
     'caller))
 
  
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
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
  ; relies upon the model-settings:global-lookup procedure to find values in the user-namespace.
  
  (define reconstruct-completed
    (contract
     (-> syntax? (lambda (x) #t) (lambda (x) #t))
     (lambda (expr value)
       ; unwinding will go here?
       (syntax-object->datum
        (kernel:kernel-syntax-case expr #f
          [(define-values vars-stx body)
           (let* ([vars (syntax->list (syntax vars-stx))]
                  [values (map model-settings:global-lookup (map syntax-e vars))]
                  [recon-vars (map recon-value values)])
             (attach-info (d->so `(define-values ,(syntax vars-stx) (values ,@recon-vars))) expr))]
          [else
           (recon-value value)])))
     'reconstruct-completed
     'caller))
  
  
                                                                                                                
                                                                                                                
                                                                                                                
                                       ;                     ;                                               ;  
 ; ;;  ;;;    ;;;   ;;;   ; ;;    ;;; ;;;; ; ;; ;   ;   ;;; ;;;;         ;;;  ;   ;  ; ;; ; ;;  ;;;   ; ;;  ;;;;
 ;;   ;   ;  ;     ;   ;  ;;  ;  ;     ;   ;;   ;   ;  ;     ;          ;     ;   ;  ;;   ;;   ;   ;  ;;  ;  ;  
 ;    ;   ;  ;     ;   ;  ;   ;  ;     ;   ;    ;   ;  ;     ;          ;     ;   ;  ;    ;    ;   ;  ;   ;  ;  
 ;    ;;;;;  ;     ;   ;  ;   ;   ;;   ;   ;    ;   ;  ;     ;   ;;;;;  ;     ;   ;  ;    ;    ;;;;;  ;   ;  ;  
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;   ;  ;     ;          ;     ;   ;  ;    ;    ;      ;   ;  ;  
 ;    ;      ;     ;   ;  ;   ;     ;  ;   ;    ;  ;;  ;     ;          ;     ;  ;;  ;    ;    ;      ;   ;  ;  
 ;     ;;;;   ;;;   ;;;   ;   ;  ;;;    ;; ;     ;; ;   ;;;   ;;         ;;;   ;; ;  ;    ;     ;;;;  ;   ;   ;;
                                                                                                                
                                                                                                                
                                                                                                                
  
  ; reconstruct-current : takes a parsed expression, a list of marks, the kind of break, and
  ; any values that may have been returned at the break point. It produces a list of sexps
  ; (the result of reconstruction) --- which may contain holes, indicated by the 
  ; highlight-placeholder --- and a list of sexps which go in the holes
  
  ; reconstruct-current : syntax (list-of mark) symbol (list-of value) -> (list (listof sexp) (listof sexp))

  (define reconstruct-current
    (contract
     (-> syntax? mark-list? break-kind? list? (listof (listof any?)))
     (lambda (expr mark-list break-kind returned-value-list)
       
       (local
           ((define (recon-top-level expr so-far)
              (kernel:kernel-syntax-case expr #f
                [(define-values vars-stx body)
                 (let* ([vars (syntax->list (syntax vars-stx))])
                   (attach-info (d->so `(define-values ,(syntax vars-stx) 
                                          ,so-far))
                                expr))]
                [else
                 so-far]))
            
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
                        (recon-source-expr expr mark-list null))]
                     [top-mark (car mark-list)]
                     [expr (mark-source top-mark)]
                     
                     [recon-let
                      (lambda ()
                        (with-syntax ([(label ((vars rhs) ...) . bodies) expr])
                          (let*-2vals ([binding-sets (map syntax->list (syntax->list (syntax (vars ...))))]
                                       [binding-list (multi-append binding-sets)]
                                       [rhs-vals (map (lambda (arg-binding) 
                                                        (mark-binding-value (lookup-binding mark-list arg-binding)))
                                                      binding-list)]
                                       [rhs-val-sets (reshape-list rhs-vals binding-sets)]
                                       [rhs-name-sets
                                        (map (lambda (binding-set)
                                               (map (lambda (binding)
                                                      (syntax-property binding
                                                                       'stepper-lifted-name
                                                                       (binding-lifted-name mark-list binding)))
                                                    binding-set))
                                             binding-sets)]
                                       [glumps (map make-let-glump rhs-name-sets (syntax->list (syntax (rhs ...))) rhs-val-sets)]
                                       [num-defns-done (mark-binding-value (lookup-binding mark-list let-counter))]
                                       [(done-glumps not-done-glumps)
                                        (n-split-list num-defns-done glumps)]
                                       [recon-lifted-val
                                        (lambda (name val)
                                          (let ([rectified-val (let-rhs-recon-value val)])
                                            (d->so `(,name ,rectified-val))))]
                                       [recon-lifted 
                                        (lambda (names expr)
                                          (d->so `(,names ,expr)))]
                                       [before-bindings
                                        (multi-append
                                         (map
                                          (lambda (glump)
                                            (let* ([rhs-val-set (let-glump-val-set glump)]
                                                   [rhs-name-set (let-glump-name-set glump)])
                                              (map recon-lifted-val rhs-name-set rhs-val-set)))
                                          done-glumps))]
                                       [reconstruct-remaining-def
                                        (lambda (glump)
                                          (let ([rhs-source (let-glump-exp glump)]
                                                [rhs-name-set (let-glump-name-set glump)])
                                            (recon-lifted rhs-name-set
                                                          (recon-source-current-marks rhs-source))))]
                                       [after-bindings
                                        (if (pair? not-done-glumps)
                                            (if (eq? so-far nothing-so-far)
                                                (map reconstruct-remaining-def not-done-glumps)
                                                (cons (recon-lifted (let-glump-name-set (car not-done-glumps)) so-far)
                                                      (map reconstruct-remaining-def (cdr not-done-glumps))))
                                            null)]
                                       [recon-bindings (append before-bindings after-bindings)]
                                       [rectified-bodies (map (lambda (body) (recon-source-expr body mark-list binding-list))
                                                              (syntax->list (syntax bodies)))])
                                      (attach-info (d->so `(,(syntax label) ,recon-bindings ,@rectified-bodies)) expr))))])
                (kernel:kernel-syntax-case expr #f 
                  ; variable references
                  [id
                   (identifier? (syntax id))
                   (if (eq? so-far nothing-so-far)
                       (recon-source-current-marks expr)
                       (error 'recon-inner "variable reference given as context: ~a" expr))]
                  
                  [(#%top . id)
                   (if (eq? so-far nothing-so-far)
                       (recon-source-current-marks expr)
                       (error 'recon-inner "variable reference given as context: ~a" expr))]
                  
                  ; applications
                  [(#%app . terms)
                   (attach-info
                    (let* ([sub-exprs (syntax->list (syntax terms))]
                           [arg-temps (build-list (length sub-exprs) get-arg-var)]
                           [arg-vals (map (lambda (arg-temp) 
                                            (mark-binding-value (lookup-binding mark-list arg-temp)))
                                          arg-temps)])
                      (case (mark-label (car mark-list))
                        ((not-yet-called)
                         (let*-2vals ([(evaluated unevaluated) (split-list (lambda (x) (eq? (cadr x) *unevaluated*))
                                                                           (zip sub-exprs arg-vals))]
                                      [rectified-evaluated (map recon-value (map cadr evaluated))])
                                     (d->so
                                      (if (null? unevaluated)
                                          rectified-evaluated
                                          `(,@rectified-evaluated
                                            ,so-far 
                                            ,@(map recon-source-current-marks (cdr (map car unevaluated))))))))
                        ((called)
                         (d->so
                          (if (eq? so-far nothing-so-far)
                              `(...) ; in unannotated code
                              `(... ,so-far ...))))
                        (else
                         (error "bad label in application mark in expr: ~a" expr))))
                    expr)]
                  
                  ; define-struct 
                  ;               
                  ;               [(z:struct-form? expr)
                  ;                 (if (comes-from-define-struct? expr)
                  ;                     so-far
                  ;                     (let ([super-expr (z:struct-form-super expr)]
                  ;                           [raw-type (utils:read->raw (z:struct-form-type expr))]
                  ;                           [raw-fields (map utils:read->raw (z:struct-form-fields expr))])
                  ;                       (if super-expr
                  ;                           `(struct (,raw-type ,so-far)
                  ;                                    ,raw-fields)
                  ;                           `(struct ,raw-type ,raw-fields))))]
                  
                  ; if
                  [(if test then else)
                   (attach-info
                    (let ([test-exp (if (eq? so-far nothing-so-far)
                                        (recon-source-current-marks (syntax test))
                                        so-far)])
                      (d->so `(if ,test-exp 
                                  ,(recon-source-current-marks (syntax then))
                                  ,(recon-source-current-marks (syntax else)))))
                    expr)]
                  
                  ; quote : there is no break on a quote.
                  
                  ; begin, begin0 : may not occur directly (or indirectly?) except in advanced
                  
                  ; let-values
                  
                  [(let-values . rest) (recon-let)]
                  
                  [(letrec-values . rest) (recon-let)]
                  
                  ; define-values : define's don't get marks, so they can't occur here
                  
                  ; lambda : there is no break on a lambda
                  
                  [else
                   (error
                    'recon-inner
                    "stepper:reconstruct: unknown object to reconstruct: ~a" (syntax-object->datum expr))])))
            
            
            (define redex #f)
            
            ; the main recursive reconstruction loop is in recon:
            ; recon : (syntax-object mark-list boolean -> syntax-object)
            
            (define (recon so-far mark-list first)
              (if (null? mark-list)
                  (recon-top-level expr so-far)
                  (let ([reconstructed (recon-inner mark-list so-far)])
                    (recon
                     (if first
                         (begin
                           (set! redex reconstructed)
                           highlight-placeholder-stx)
                         reconstructed)
                     (cdr mark-list)
                     #f))))
            
            ; we're turning off lifting, for the moment:
            
            ;         (define (rectify-let-values-step)
            ;           (let*-values ([(redex) (recon-source-expr (mark-source (car mark-list)) mark-list)]
            ;                         [(before-step) (recon null highlight-placeholder-stx (cdr mark-list) #f)]
            ;                         [(r-before r-after reduct) (recon-inner mark-list nothing-so-far)]
            ;                         [(new-defs) (append r-before r-after)]
            ;                         [(after-step) (recon (build-list (length new-defs) 
            ;                                                          (lambda (x) highlight-placeholder-stx))
            ;                                              highlight-placeholder-stx
            ;                                              (cdr mark-list) 
            ;                                              #f)])
            ;             (append (unwind before-step (list redex))
            ;                     (unwind after-step (append new-defs (list reduct))))))
            
            (define answer
              (map (lambda (x) (map syntax-object->datum x))
                   (case break-kind
                     ((result-value-break result-exp-break)
                      (let* ([innermost (if (null? returned-value-list) ; is it an expr -> expr reduction?
                                            (recon-source-expr (mark-source (car mark-list)) mark-list null)
                                            (recon-value (car returned-value-list)))]
                             [recon-expr (recon highlight-placeholder-stx (cdr mark-list) #f)])
                        (unwind (list recon-expr) (list innermost))))
                     ((normal-break)
                      (let ([recon-expr (recon nothing-so-far mark-list #t)])
                        (unwind (list recon-expr) (list redex))))
                     ((double-break late-let-break)
                      (error 'answer "lifting turned off"))
                     ;                  ((double-break)
                     ;                   (rectify-let-values-step))
                     ;                  ((late-let-break)
                     ;                   (let-values ([(before after junk) (recon-inner mark-list nothing-so-far)])
                     ;                     (unless (null? after)
                     ;                       (error 'answer "non-empty 'after' defs in late-let-break: ~a" (map syntax-object->datum after)))
                     ;                     (list before)))
                     (else
                      (error 'reconstruct-current-def "unknown break kind: " break-kind)))))
            
            )
         
         answer))
     'reconstruct-module
     'caller)))
