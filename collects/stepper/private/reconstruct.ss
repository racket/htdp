; general assertions about reconstruction:
; a varref can only occur at the top of a mark-list
; a varref at the top of the mark-list must either be a top-level-variable
;  or have a value in some mark somewhere (or both).

(module reconstruct mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "list.ss")
           (lib "etc.ss")
	   (lib "contract.ss")
           "marks.ss"
           "model-settings.ss"
           "shared.ss"
           "highlight-placeholder.ss"
           "display-exp-interface.ss"
           "my-macros.ss"
           "lifting.ss")

  (provide/contract 
   [reconstruct-completed (-> syntax? any? render-settings? any)]
   [reconstruct-current (-> syntax? mark-list? symbol? (listof any?) render-settings?
       (or/f (listof exp-without-holes?)
             (list/p (listof exp-with-holes?) (listof exp-without-holes?))
             (list/p (listof exp-with-holes?) (listof exp-without-holes?) 
                     (listof exp-with-holes?) (listof exp-without-holes?))))]
   [final-mark-list? (-> mark-list? boolean?)]
   [skip-step? (-> break-kind? mark-list? render-settings? boolean?)])
  
  (define the-undefined-value (letrec ([x x]) x))
  
  (define nothing-so-far (gensym "nothing-so-far-"))

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
  
  (define (recon-value val render-settings . hint-list)
    (let ([hint (if (pair? hint-list) (car hint-list))]
          [closure-record (closure-table-lookup val (lambda () #f))])
      (cond
        [closure-record
         (cond [(and (not (eq? hint 'let-rhs))
                     (closure-record-name closure-record)) =>
                     (lambda (name)
                       (cond [(closure-record-lifted-name closure-record) =>
                              (lambda (lifted-name)
                                #`#,(construct-lifted-name #`#,name lifted-name))]
                             [else #`#,name]))]
               [else
                (let ([mark (closure-record-mark closure-record)])
                  (recon-source-expr (mark-source mark) (list mark) null render-settings))])]
        [else
         (let ([rendered ((render-settings-render-to-sexp render-settings) val)])
           (if (symbol? rendered)
               #`#,rendered
               #`(#%datum . #,rendered)))])))
  
  (define (let-rhs-recon-value val render-settings)
    (recon-value val render-settings 'let-rhs))
  
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
                                                                                         
  (define (skip-step? break-kind mark-list render-settings)
    (case break-kind
      [(result-value-break result-exp-break)
       #f]
      [(normal-break)
       (skip-redex-step? mark-list render-settings)]
      [(double-break)
       (or (not (render-settings-lifting? render-settings))
           (eq? (syntax-property (mark-source (car mark-list)) 'stepper-hint) 'comes-from-or))]
      [(late-let-break)
       (eq? (syntax-property (mark-source (car mark-list)) 'stepper-hint) 'comes-from-or)]))  
  
  (define (skip-redex-step? mark-list render-settings)
    (and (pair? mark-list)
         (let ([expr (mark-source (car mark-list))])
           (or (kernel:kernel-syntax-case expr #f
                  [id
                   (identifier? expr)
                   (or (eq? (syntax-property expr 'stepper-binding-type) 'lambda-bound) ; don't halt for lambda-bound vars
                       (let ([val (lookup-binding mark-list expr)]) 
                         (and (procedure? val)
                              (not (continuation? val)))))] ; don't halt for varrefs bound to non-continuation procedures
                  [(#%top . id-stx)
                   (let ([id (syntax id-stx)])
                     (with-handlers
                         ([exn:variable? (lambda args #f)]) ; DO halt for unbound top-level varrefs
                       (let ([val (global-lookup (syntax-e id))])
                         (or (and (procedure? val)                     ; don't halt for top-level procedure refs ...
                                  (eq? (syntax-e id) (object-name val)) ; with the right inferred name
                                  
                                  ; Do we need this stuff for lifted names? :
                                  
;                                  (or (not (closure-table-lookup val (lambda () #f))) ; that are primitives, or ...
;                                      (and (not (continuation? val))
;                                           (cond [(closure-table-lookup val (lambda () #f)) => ; are user fns with the right (original) name
;                                                  (lambda (x)
;                                                    (eq? id (closure-record-name x)))] ; has wrong name
;                                                 [else #f])))
                                  )))))]
                  [(#%app . terms)
                   ; don't halt for proper applications of constructors
                   (let ([fun-val (lookup-binding mark-list (get-arg-var 0))])
                     (and (procedure? fun-val)
                          (procedure-arity-includes? 
                           fun-val
                           (length (cdr (syntax->list (syntax terms)))))
                          (or (and (render-settings-constructor-style-printing? render-settings)
                                   (if (render-settings-abbreviate-cons-as-list? render-settings)
                                       (eq? fun-val (find-special-value 'list '(3)))    
                                       (and (eq? fun-val (find-special-value 'cons '(3 null)))
                                            (second-arg-is-list? mark-list))))
                              ;(model-settings:special-function? 'vector fun-val)
                              (and (eq? fun-val void)
                                   (eq? (cdr (syntax->list (syntax terms))) null))
                              (struct-constructor-procedure? fun-val))))]
                  [else
                   #f])))))
  
  (define (find-special-value name valid-args)
    (let ([expanded (car (syntax-e (cdr (syntax-e (expand (cons name valid-args))))))])
      '(fprintf (current-error-port) "identifier-binding: ~e\n" 
		(identifier-binding expanded))
      (eval expanded)))
  
  (define (second-arg-is-list? mark-list)
    (let ([arg-val (lookup-binding mark-list (get-arg-var 2))])
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
  
  ; construct-lifted-name 
  ; (-> syntax? (or/f false? num?) symbol?)
  
  (define (construct-lifted-name binding dynamic-index)
    (if dynamic-index
        (string->symbol
         (string-append (symbol->string (syntax-e binding)) "_" 
                        (number->string dynamic-index)))
        (syntax-e binding)))

  ; binding-lifted-name ((listof mark) SYNTAX-OBJECT -> num)
  
  (define (binding-lifted-name mark-list binding)
      (construct-lifted-name binding (lookup-binding mark-list (get-lifted-var binding))))

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
  
  ; unwind takes a syntax object with a single highlight-placeholder and a syntax object
  ; that represents the highlight, and returns a list of syntax objects and a list of
  ; highlights
  ; (->* (sexp-with-highlights? sexp-without-highlights?)
  ;      ((listof sexp-with-highlights) (listof sexp-without-highlights?)))
  
  (define (unwind stx highlight lift-at-highlight?)
    (let*-values ([(stx-l-lst highlight-l-lst) (lift stx highlight lift-at-highlight?)])
      (macro-unwind stx-l-lst highlight-l-lst)))
  
  ; unwind-no-highlight is really just macro-unwind, but with the 'right' interface that
  ; makes it more obvious what it does.
  ; [unwind-no-highlight (-> syntax? syntax?)]
  
  (define (unwind-no-highlight stx)
    (let-values ([(stxs _) (macro-unwind (list stx) null)])
      stxs))
  
  (define (first-of-one x) 
    (unless (= (length x) 1)
      (error 'first-of-one "expected a list of length one in: ~v" x))
    (car x))
  
  ;(->* (syntax? (listof syntax?)) 
  ;     (syntax? (listof syntax?)))
  
  (define (macro-unwind stxs highlights)
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
               (let ([fall-through
                      (lambda ()
                        (kernel:kernel-syntax-case stx #f
                          [(#%app exp ...)
                           (recur-on-pieces #'(exp ...))]
                          [(#%datum . datum)
                           #'datum]
                          [(let-values . rest)
                           (unwind-mz-let stx)]
                          [(letrec-values . rest)
                           (unwind-mz-let stx)]
                          [else
                           (recur-on-pieces stx)]))])
               (if (syntax-property stx 'user-stepper-hint)
                   (case (syntax-property stx 'user-stepper-hint)
                     [(lambda-define non-lambda-define)
                      (unwind-define stx)]
                     
                     [(shortened-proc-define)
                      (kernel:kernel-syntax-case stx #f
                        [(define-values (name . others) 
                           (lambda arglist body ...))
                         (unless (null? (syntax-e #'others))
                           (error 'reconstruct "reconstruct fails on multiple-values define\n"))
                         #`(define (name . arglist) body ...)]
                        [else (error 'macro-unwind "unexpected shape for expression: ~v with hint ~v" 
                                     (syntax-object->datum stx) 
                                     (syntax-property stx 'user-stepper-hint))])]
                     
                     [(comes-from-cond) (unwind-cond stx 
                                                     (syntax-property stx 'user-source)
                                                     (syntax-property stx 'user-position))]
                     
                     [(comes-from-and) (unwind-and/or stx
                                                      (syntax-property stx 'user-source)
                                                      (syntax-property stx 'user-position)
                                                      'and)]
                     
                     [(comes-from-or) (unwind-and/or stx
                                                     (syntax-property stx 'user-source)
                                                     (syntax-property stx 'user-position)
                                                     'or)]
                     
                     [(comes-from-local)
                      (unwind-local stx)]
                     
                     [(comes-from-let)
                      (kernel:kernel-syntax-case stx #f
                        [(define-values . rest) 
                         (unwind-define stx)]
                        [(let-values . rest)
                         (with-syntax ([(let ([tmp rhs] ...) (local ((define var dc) ...) body)) (unwind-mz-let stx)])
                           #`(let ([var rhs] ...) body))]
                        [else (error 'unwind-macro "unexpected form for comes-from-let: ~v\n" (syntax-object->datum stx))])]
                     
                     ((quasiquote-the-cons-application) (unwind-quasiquote-the-cons-application stx))
                     
                     (else (fall-through)))
                   (fall-through)))))
         
         (define (unwind-define stx)
           (kernel:kernel-syntax-case stx #f
             [(define-values (name . others) body)
              (unless (null? (syntax-e #'others))
                (error 'reconstruct "reconstruct fails on multiple-values define: ~v\n" (syntax-object->datum stx)))
              (let* ([orig-name (or (syntax-property #'name 'stepper-orig-name)
                                    #'name)]
                     [lifted-name (if (syntax-property stx 'stepper-lifted-name)
                                      (construct-lifted-name orig-name (syntax-property #'name 'stepper-lifted-name))
                                      orig-name)])
                #`(define #,lifted-name #,(inner #'body)))]
             [else (error 'macro-unwind "unexpected shape for expression: ~v with hint ~v" 
                          (syntax-object->datum stx) 
                          (syntax-property stx 'user-stepper-hint))]))
         (define (unwind-mz-let stx)
           (with-syntax ([(label ([(var) rhs] ...) . bodies) stx])
             (with-syntax ([(rhs2 ...) (map inner (syntax->list #'(rhs ...)))]
                           [new-label (case (syntax-e #'label)
                                        [(let-values) #'let]
                                        [(letrec-values) #'letrec])])
               #`(let ([var rhs2] ...) . #,(map inner (syntax->list #'bodies))))))
         
         (define (unwind-local stx)
           (kernel:kernel-syntax-case stx #f
             [(letrec-values ([(var) exp] ...) body) ; at least through intermediate, define-values may not occur in local.
              (with-syntax ([(exp2 ...) (map inner (syntax->list #'(exp ...)))])
                #`(local ((define var exp2) ...) #,(inner #'body)))]
             [(define-values (var) body)
              #`(define (var) body)]))
         
         (define (unwind-quasiquote-the-cons-application stx)
           (syntax-case (recur-on-pieces stx) ()
             [(#%app the-cons . rest)
              (syntax (cons . rest))]
             [else
              (error 'reconstruct "unexpected result for unwinding the-cons application")]))
         
         (define (unwind-cond stx user-source user-position)
           (if (eq? stx highlight-placeholder-stx)
               (begin (queue-push highlight-queue-dest (unwind-cond (queue-pop highlight-queue-src) user-source user-position))
                      highlight-placeholder-stx)
               (with-syntax ([clauses
                              (let loop ([stx stx])
                                (if (and (eq? user-source (syntax-property stx 'user-source))
                                         (eq? user-position (syntax-property stx 'user-position)))
                                    (syntax-case stx (if begin #%app)
                                      [(if test result else-clause)
                                       (with-syntax ([new-test (if (syntax-property stx 'user-stepper-else)
                                                               (syntax else)
                                                               (inner (syntax test)))]
                                                     [result (inner (syntax result))])
                                         (cons (syntax (new-test result))
                                               (loop (syntax else-clause))))]
                                      [(error . args)
                                       null]
                                      [else-stx
                                       (error 'unwind-cond "expected an if or a begin, got: ~e" (syntax-object->datum (syntax else-stx)))])
                                    (error 'unwind-cond "expected a cond clause expansion, got: ~e" (syntax-object->datum stx))))])
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
      (let* ([main (map inner stxs)]
             [new-highlights (build-list (queue-length highlight-queue-dest) (lambda (x) (queue-pop highlight-queue-dest)))])
        (values main new-highlights))))
  
  
                                                                       
                                                                                                               
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
     (-> syntax? mark-list? binding-set? render-settings? syntax?)
     (lambda (expr mark-list lexically-bound-bindings render-settings)
      (if (syntax-property expr 'stepper-skipto)
          (skipto-reconstruct
           (syntax-property expr 'stepper-skipto)
           expr
           (lambda (stx)
             (recon-source-expr stx mark-list lexically-bound-bindings render-settings)))
          (if (syntax-property expr 'stepper-prim-name)
              (syntax-property expr 'stepper-prim-name)
              (let* ([recur (lambda (expr) (recon-source-expr expr mark-list lexically-bound-bindings render-settings))]
                     [let-recur (lambda (expr bindings)
                                  (recon-source-expr expr mark-list (append bindings lexically-bound-bindings) render-settings))]
                     
                     [recon-basic
                      (lambda ()
                        (with-syntax ([(label . bodies) expr])
                          #`(label #,@(map recur (syntax->list (syntax bodies))))))]
                     [recon-let/rec
                      (lambda ()
                        (with-syntax ([(label  ((vars val) ...) body) expr])
                          (let* ([bindings (map syntax->list (syntax->list (syntax (vars ...))))]
                                 [binding-list (apply append bindings)]
                                 [right-sides (map recur (syntax->list (syntax (val ...))))]
                                 [recon-body (let-recur (syntax body) binding-list)])
                            (with-syntax ([(recon-val ...) right-sides]
                                          [recon-body recon-body])
                              (syntax (label ((vars recon-val) ...) recon-body))))))]
                     [recon-lambda-clause
                      (lambda (clause)
                        (with-syntax ([(args . bodies-stx) clause])
                          (let* ([arglist (arglist-flatten #'args)]
                                 [bodies (map (lambda (body) (let-recur body arglist))
                                              (syntax->list (syntax bodies-stx)))])
                            (cons (syntax args) bodies))))]
                     [recon (kernel:kernel-syntax-case expr #f
                              
                              ; lambda
                              [(lambda . clause-stx)
                               (let* ([clause (recon-lambda-clause (syntax clause-stx))])
                                 #`(lambda #,@clause))]
                              
                              ; case-lambda
                              [(case-lambda . clauses-stx)
                               (let* ([clauses (map recon-lambda-clause (syntax->list (syntax clauses-stx)))])
                                 #`(case-lambda #,@clauses))]
                              
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
                              [(quote body) (recon-value (syntax-e (syntax body)) render-settings)]
                              
                              ; quote-syntax : like set!, the current stepper cannot handle quote-syntax
                              
                              ; with-continuation-mark
                              [(with-continuation-mark . rest) (recon-basic)]
                              
                              ; application
                              [(#%app . terms) (recon-basic)]
                              
                              ; #%datum
                              [(#%datum . datum) 
                               #`#,(recon-value (syntax-e #'datum) render-settings)]
                              
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
                                               (recon-value (lookup-binding mark-list var) render-settings))
                                              ((macro-bound)
                                               ; for the moment, let-bound vars occur only in and/or :
                                               (recon-value (lookup-binding mark-list var) render-settings))
                                              ; #`#,(binding-lifted-name mark-list var))
                                              ((top-level) var)
                                              ((let-bound)
                                               (syntax-property var
                                                                'stepper-lifted-name
                                                                (binding-lifted-name mark-list var)))
                                              ((stepper-temp)
                                               (error 'recon-source-expr "stepper-temp showed up in source?!?"))
                                              (else
                                               (error 'recon-source-expr "unknown 'stepper-binding-type property: ~a" 
                                                      (syntax-property var 'stepper-binding-type)))))]
                                       [else ; top-level-varref
                                        (fixup-name
                                         var)]))]
                              [(#%top . var)
                               (syntax var)]
                              
                              [else
                               (error 'recon-source "no matching clause for syntax: ~a" expr)])])
                (attach-info recon expr)))))
     'recon-source-expr
     'caller))
 
  
  ;; mflatt: MAJOR HACK - work around the prefix on
  ;;         beginner name definitions
  (define (fixup-name s)
    (let ([m (regexp-match re:beginner: (symbol->string (syntax-e s)))])
      (if m
	  (datum->syntax-object s (string->symbol (cadr m)) s s)
	  s)))
  (define re:beginner: (regexp "^beginner:(.*)$"))
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
  
  (define (reconstruct-completed expr value render-settings)
    (cond 
      [(syntax-property expr 'stepper-skipto) =>
       (lambda (skipto)
         (skipto-reconstruct skipto expr
                             (lambda (expr)
                               (reconstruct-completed expr value render-settings))))]
      [(syntax-property expr 'stepper-define-struct-hint) =>
       (lambda (define-struct-info)
         (syntax-object->datum (cadr define-struct-info)))]
      [else
       (syntax-object->datum
        (first-of-one (unwind-no-highlight
                       (kernel:kernel-syntax-case expr #f
                         [(define-values vars-stx body)
                          (let* ([vars (syntax->list #'vars-stx)]
                                 [values (map global-lookup (map syntax-e vars))]
                                 [recon-vals (map (lambda (val var) 
                                                    (if (procedure? val)
                                                        (reconstruct-completed-procedure 
                                                         val
                                                         (construct-lifted-name var (syntax-property var 'stepper-lifted-name))
                                                         render-settings)
                                                        (recon-value val render-settings))) 
                                                  values
                                                  vars)])
                            (if (= (length recon-vals) 1)
                                (attach-info #`(define-values vars-stx #,(car recon-vals)) expr)
                                (attach-info #'(define-values vars-stx (values #,@recon-vals)) expr)))]
                         [else
                          (recon-value value render-settings)]))))]))
  
  ; : (-> syntax? syntax? sexp?)
  ; DESPERATELY SEEKING ABSTRACTION
  (define (reconstruct-top-level source reconstructed)
    (cond 
      [(syntax-property source 'stepper-skipto) =>
       (lambda (skipto)
         (skipto-reconstruct skipto source
                             (lambda (expr)
                               (reconstruct-top-level expr reconstructed))))]
      [else
       (kernel:kernel-syntax-case source #f
          [(define-values vars-stx body)
           (attach-info #`(define-values vars-stx #,reconstructed)
                        source)]
          [else
           reconstructed])]))
  
  ; : (-> procedure? symbol? render-settings? syntax?)
  ; constructs the lambda expression for a closure
  (define (reconstruct-completed-procedure val assigned-name render-settings)
    (let* ([closure-record (closure-table-lookup val 
                                                 (lambda () 
                                                   #f))])
      (if closure-record  
          (let* ([mark (closure-record-mark closure-record)]
                 [base-name (closure-record-name closure-record)]
                 [name (if base-name
                           (construct-lifted-name #`#,base-name (closure-record-lifted-name closure-record))
                           #f)])
            (if (eq? name assigned-name)
                (first-of-one (unwind-no-highlight (recon-source-expr (mark-source mark) (list mark) null render-settings)))
                #`#,name))
          #`#,((render-settings-render-to-sexp render-settings) val))))

                                                                                                                
                                                                                                                
                                                                                                                
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
  
  (define (reconstruct-current expr mark-list break-kind returned-value-list render-settings)
    (local
        (
         
         ; ;;  ;;;    ;;;   ;;;   ; ;;          ;  ; ;;   ; ;;    ;;;   ; ;;
         ;;   ;   ;  ;     ;   ;  ;;  ;         ;  ;;  ;  ;;  ;  ;   ;  ;;  
         ;    ;   ;  ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;   ;  ;   
         ;    ;;;;;  ;     ;   ;  ;   ;  ;;;;;  ;  ;   ;  ;   ;  ;;;;;  ;   
         ;    ;      ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;      ;   
         ;    ;      ;     ;   ;  ;   ;         ;  ;   ;  ;   ;  ;      ;   
         ;     ;;;;   ;;;   ;;;   ;   ;         ;  ;   ;  ;   ;   ;;;;  ;   
         
         
         (define (recon-inner mark-list so-far)
           (let* ([recon-source-current-marks 
                   (lambda (expr)
                     (recon-source-expr expr mark-list null render-settings))]
                  [top-mark (car mark-list)]
                  [expr (mark-source top-mark)]
                  
                  [recon-let
                   (lambda ()
                     (with-syntax ([(label ((vars rhs) ...) . bodies) expr])
                       (let*-2vals ([binding-sets (map syntax->list (syntax->list #'(vars ...)))]
                                    [binding-list (apply append binding-sets)]
                                    [glumps 
                                     (map (lambda (binding-set rhs)
                                            (make-let-glump
                                             (map (lambda (binding)
                                                   (syntax-property binding
                                                                    'stepper-lifted-name
                                                                     (binding-lifted-name mark-list binding)))
                                                 binding-set)
                                             rhs
                                             (map (lambda (arg-binding) 
                                                     (lookup-binding mark-list arg-binding))
                                                  binding-set)))
                                          binding-sets
                                          (syntax->list #`(rhs ...)))]
                                    [num-defns-done (lookup-binding mark-list let-counter)]
                                    [(done-glumps not-done-glumps)
                                     (n-split-list num-defns-done glumps)]
                                    [recon-lifted-val
                                     (lambda (name val)
                                       (let ([rectified-val (let-rhs-recon-value val render-settings)])
                                         #`(#,name #,rectified-val)))]
                                    [recon-lifted 
                                     (lambda (names expr)
                                       #`(#,names #,expr))]
                                    [before-bindings
                                     (map
                                      (lambda (glump)
                                        (let* ([rhs-val-set (map (lambda (val)
                                                                   (let-rhs-recon-value val render-settings)) 
                                                                 (let-glump-val-set glump))]
                                               [rhs-name-set (let-glump-name-set glump)])
                                          (if (= (length rhs-val-set) 1)
                                              #`(#,rhs-name-set #,@rhs-val-set)
                                              #`(#,rhs-name-set (values #,rhs-val-set)))))
                                      done-glumps)]
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
                                    [rectified-bodies (map (lambda (body) (recon-source-expr body mark-list binding-list render-settings))
                                                           (syntax->list (syntax bodies)))])
                         (attach-info #`(label #,recon-bindings #,@rectified-bodies) expr))))])
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
                                         (lookup-binding mark-list arg-temp))
                                       arg-temps)])
                   (case (mark-label (car mark-list))
                     ((not-yet-called)
                      (let*-2vals ([(evaluated unevaluated) (split-list (lambda (x) (eq? (cadr x) *unevaluated*))
                                                                        (zip sub-exprs arg-vals))]
                                   [rectified-evaluated (map (lx (recon-value _ render-settings)) (map cadr evaluated))])
                        (if (null? unevaluated)
                            #`(#%app . #,rectified-evaluated)
                            #`(#%app 
                               #,@rectified-evaluated
                               #,so-far 
                               #,@(map recon-source-current-marks (cdr (map car unevaluated)))))))
                     ((called)
                      (if (eq? so-far nothing-so-far)
                          (datum->syntax-object #'here `(,#'#%app ...)) ; in unannotated code
                          (datum->syntax-object #'here `(,#'#%app ... ,so-far ...))))
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
                                     (recon-value (lookup-binding mark-list if-temp) render-settings)
                                     so-far)])
                   #`(if #,test-exp 
                         #,(recon-source-current-marks (syntax then))
                         #,(recon-source-current-marks (syntax else))))
                 expr)]
               
               ; one-armed if
               
               [(if test then)
                (attach-info
                 (let ([test-exp (if (eq? so-far nothing-so-far)
                                     (recon-value (lookup-binding mark-list if-temp) render-settings)
                                     so-far)])
                   #`(if #,test-exp 
                         #,(recon-source-current-marks (syntax then))))
                 expr)]
               
               ; quote : there is no break on a quote.
               
               ; begin : may not occur directly, but will occur in the expansion of cond, now that I'm no longer
               ; masking that out with stepper-skipto. Furthermore, exactly one expression can occur inside it.
               
               [(begin clause)
                (attach-info
                 (if (eq? so-far nothing-so-far)
                     #`(begin #,(recon-source-current-marks (syntax clause)))
                     (error 
                      'recon-inner
                      "stepper:reconstruct: one-clause begin appeared as context: ~a" (syntax-object->datum expr)))
                 expr)]
               
               ; begin0 : may not occur directly except in advanced
               
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
               (reconstruct-top-level expr so-far)
               (let ([reconstructed (recon-inner mark-list so-far)])
                 (recon
                  (if first
                      (begin
                        (set! redex reconstructed)
                        highlight-placeholder-stx)
                      reconstructed)
                  (cdr mark-list)
                  #f))))
         
         (define answer
           (map (lambda (x) (map syntax-object->datum x))
                (case break-kind
                  ((result-value-break result-exp-break)
                   (let* ([innermost (if (null? returned-value-list) ; is it an expr -> expr reduction?
                                         (recon-source-expr (mark-source (car mark-list)) mark-list null render-settings)
                                         (recon-value (car returned-value-list) render-settings))]
                          [recon-expr (recon highlight-placeholder-stx (cdr mark-list) #f)])
                     (let-values ([(a b) (unwind recon-expr innermost #f)])
                       (list a b))))
                  ((normal-break)
                   (let ([recon-expr (recon nothing-so-far mark-list #t)])
                     (let-values ([(a b) (unwind recon-expr redex #f)])
                       (list a b))))
                  ((double-break)
                   (let ([recon-expr (recon nothing-so-far mark-list #t)])
                     (let-values ([(before-stxs before-highlights) (unwind recon-expr redex #f)]
                                  [(after-stxs after-highlights) (unwind recon-expr redex #t)])
                       (list before-stxs before-highlights after-stxs after-highlights))))
                  ((late-let-break)
                   (let* ([one-level-recon (unwind-no-highlight (recon-inner mark-list nothing-so-far))])
                     (sublist 0 (- (length one-level-recon) 1) one-level-recon)))
                  (else
                   (error 'reconstruct-current-def "unknown break kind: " break-kind)))))
         
         )
      
      answer)))
