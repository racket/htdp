(module annotate mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
	   (lib "list.ss")
           (lib "etc.ss")
           "marks.ss"
           "shared.ss"
           "my-macros.ss")

  (provide
   initial-env-package
   annotate
   top-level-rewrite
   )
 
  ;;                                              ;;;;                          ;                     
 ;  ;                                     ;       ;                         ;                         
 ;     ;   ;  ; ;;;   ; ;;;    ;;;   ; ;;;;;;     ;     ;   ;  ; ;;    ;;; ;;;; ;   ;;;   ; ;;    ;;; 
 ;     ;   ;  ;;   ;  ;;   ;  ;   ;  ;;   ;       ;     ;   ;  ;;  ;  ;     ;   ;  ;   ;  ;;  ;  ;    
  ;;   ;   ;  ;    ;  ;    ;  ;   ;  ;    ;       ;;;;  ;   ;  ;   ;  ;     ;   ;  ;   ;  ;   ;  ;    
    ;  ;   ;  ;    ;  ;    ;  ;   ;  ;    ;       ;     ;   ;  ;   ;  ;     ;   ;  ;   ;  ;   ;   ;;  
    ;  ;   ;  ;    ;  ;    ;  ;   ;  ;    ;       ;     ;   ;  ;   ;  ;     ;   ;  ;   ;  ;   ;     ; 
 ;  ;  ;  ;;  ;;   ;  ;;   ;  ;   ;  ;    ;       ;     ;  ;;  ;   ;  ;     ;   ;  ;   ;  ;   ;     ; 
  ;;    ;; ;  ; ;;;   ; ;;;    ;;;   ;     ;;     ;      ;; ;  ;   ;   ;;;   ;; ;   ;;;   ;   ;  ;;;  
              ;       ;                                                                               
              ;       ;                                                                               
                                                                                                      
  ;; mapmap : maps the fn across the sub-lists
  (define (mapmap fn lolst)
    (map (lambda (x) (map fn x)) lolst))
  

  
  ;; this looks wrong...
  (define (internal-error . x)
    (error 'annotater-internal-error "~s" x))

  ; gensyms for annotation:
  
  ; the mutator-gensym is used in building the mutators that go into certain marks.
  ; (define mutator-gensym (gensym "mutator-"))
  
  ; the `closure-temp' symbol is used for the let which wraps created closures, so
  ; that we can stuff them into the hash table.
  
  ; closure-temp: uninterned-symbol
  
  (define closure-temp (gensym "closure-temp-"))
  
  ; 2vals-map : (('a -> (2vals 'b 'c)) ('a list)) -> (2vals ('b list) ('c list))
  ;  dual-map is like map, only for a procedure that returns (values a b), and its
  ;  result is (values a-list b-list)... the contract specifies this more clearly.
  
  (define (2vals-map f . lsts)
    (if (null? (car lsts))
        (2vals null null)
        (let*-2vals ([(a b) (apply f (map car lsts))]
                     [(a-rest b-rest) (apply 2vals-map f (map cdr lsts))])
          (2vals (cons a a-rest) (cons b b-rest)))))
  
  ; triple-map (('a -> (values 'b 'c 'd)) ('a list)) -> (values ('b list) ('c list) ('d list))
  
  (define (triple-map f . lsts)
    (letrec ([inr (lambda lsts
                    (if (null? (car lsts))
                        (values null null null)
                        (let*-values
                            ([(a b c) (apply f (map car lsts))]
                             [(a-rest b-rest c-rest) (apply inr (map cdr lsts))])
                          (values (cons a a-rest) (cons b b-rest) (cons c c-rest)))))])
      (apply inr lsts)))

  ; note: a BINDING-SET which is not 'all may be used as a VARREF-SET.
  ; this is because they both consist of syntax objects, and a binding
  ; answers true to bound-identifier=? with itself, just like a varref
  ; in the scope of that binding would.
  
  ; binding-set-union: (listof BINDING-SET) -> BINDING-SET
  ; varref-set-union: (listof VARREF-SET) -> VARREF-SET
  
  (define-values (binding-set-union
                  varref-set-union)
    (local ((define (set-pair-union a-set b-set comparator)
              (append (remove* a-set b-set comparator) a-set))
            
            (define (varref-set-pair-union a-set b-set)
              (set-pair-union a-set b-set free-identifier=?))
            
            (define (binding-set-pair-union a-set b-set)
              (cond [(eq? a-set 'all) 'all]
                    [(eq? b-set 'all) 'all]
                    [else (set-pair-union a-set b-set eq?)]))
            
            (define (pair-union->many-union fn)
              (lambda (args)
                (foldl fn null args)))
            
            (define binding-set-union
              (pair-union->many-union binding-set-pair-union))
            
            (define varref-set-union
              (pair-union->many-union varref-set-pair-union)))
      (values binding-set-union
              varref-set-union)))

  ; binding-set-varref-set-intersect : BINDING-SET VARREF-SET -> BINDING-SET
  ; return the subset of varrefs that appear in the bindings
  
  (define (binding-set-varref-set-intersect bindings varrefs)
    (cond [(eq? bindings 'all) varrefs]
          [else (filter (lambda (varref)
                          (ormap (lambda (binding)
                                   (bound-identifier=? binding varref))
                                 bindings))
                        varrefs)]))
  
  ; varref-set-remove-bindings : VARREF-SET (BINDING-SET - 'all) -> VARREF-SET
  ; remove bindings from varrefs
  
  (define (varref-set-remove-bindings varrefs bindings)
    (cond [(eq? bindings 'all)
           (error 'varref-set-remove-bindings "binding-set 'all passed as second argument, first argument was: ~s" varrefs)]
          [else (remove* bindings varrefs bound-identifier=?)]))
      
  (define (interlace a b)
    (foldr (lambda (a b built)
             (cons a (cons b built)))
           null
           a
           b))
        
  (define (closure-key-maker closure)
    closure)
  
  ;;;;;;;;;;
  ;;
  ;; make-debug-info builds the thunk which will be the mark at runtime.  It contains 
  ;; a source expression and a set of binding/value pairs.
  ;; (syntax-object BINDING-SET VARREF-SET symbol boolean) -> debug-info)
  ;;
  ;;;;;;;;;;
     
  (define make-debug-info
    (checked-lambda (source (tail-bound BINDING-SET) (free-vars VARREF-SET) label lifting?)
                    (let*-2vals ([kept-vars (binding-set-varref-set-intersect tail-bound free-vars)]
                                 [var-clauses (map (lambda (x) 
                                                     (list x (d->so `(quote-syntax ,x))))
                                                   kept-vars)]
                                 [let-bindings (filter (lambda (var) 
                                                         (case (syntax-property var 'stepper-binding-type)
                                                           ((let-bound) #t)
                                                           ((lambda-bound stepper-temp non-lexical) #f)
                                                           (else (error 'make-debug-info 
                                                                        "varref ~a's binding-type info was not recognized: ~a"
                                                                        (syntax-e var)
                                                                        (syntax-property var 'stepper-binding-type)))))
                                                       kept-vars)]
                                 [lifter-syms (map get-lifted-var let-bindings)]
                                 [quoted-lifter-syms (map (lambda (b) 
                                                            (d->so `(quote-syntax ,b))) 
                                                          lifter-syms)]
                                 [let-clauses (map list lifter-syms quoted-lifter-syms)])
                      (make-full-mark source label (append var-clauses (if lifting? let-clauses null))))))
  
  ; cheap-wrap for non-debugging annotation
  
  (define (cheap-wrap body)
    (d->so `(with-continuation-mark ,debug-key
                                       ,(make-cheap-mark (d->so `(quote-syntax ,body)))
                                       ,body)))
  
  ; wrap-struct-form 
  
;  (define (wrap-struct-form names annotated)
;    (let* ([arg-temps (build-list (length names) get-arg-var)]
;           [struct-proc-names (cdr names)]
;           [closure-records (map (lambda (proc-name) (make-closure-record
;                                                      proc-name 
;                                                      (lambda () #f)
;                                                      (eq? proc-name (car struct-proc-names))
;                                                      #f))
;                                 struct-proc-names)]
;           [proc-arg-temp-syms (cdr arg-temp-syms)]
;           [setters (map (lambda (arg-temp-sym closure-record)
;                           `(,closure-table-put! ,arg-temp-sym ,closure-record))
;                         proc-arg-temp-syms
;                         closure-records)]
;           [full-body (append setters (list `(values ,@arg-temp-syms)))])
;      `(#%let-values ((,arg-temp-syms ,annotated)) ,@full-body)))
  
  (define initial-env-package (list null null 0))
  
  (define (extract-top-level-vars exprs)
    (apply append
           (map (lambda (expr)
                  (syntax-case expr (define-values)
                    [(define-values vars body)
                     (syntax->list (syntax vars))]
                    [else
                     null]))
                exprs)))
  
  (define (term-is-reduced stx)
    (syntax-case stx (quote quote-syntax #%top)
      [(quote _) #t]
      [(quote-syntax _) #t]
      [(#%top . _) #t]
      [else (symbol? (syntax-e stx))]))
  
  ;;;;;;;;;;
  ;;
  ;; collapse-let-values: for the purposes of the annotater, it's easier to simply collapse let's and
  ;;  let*'s into one big let*.  The lexical-binding information attached to each variable reference
  ;;  guarantees that this won't 
  ;;
  ;;;;;;;;;;
  
  (define (collapse-let-values stx)
    (syntax-case stx (let-values let*-values)
      [(_ (outer-binding ...) (let-values (inner-binding ...) . bodies))
       (collapse-let-values (syntax/loc stx (let*-values (outer-binding ... inner-binding ...) . bodies)))]
      [else stx]))

  ; test exprs:
  ;  (andmap (lambda (arg-list)
  ;            (let* ([stx (car arg-list)]
  ;                   [elaborated (cadr arg-list)]
  ;                   [eval-result (caddr arg-list)]
  ;                   [collapsed (collapse-let-values (expand stx))])
  ;              (printf "~a~n~a~n~a~n~a~n" (syntax-object->datum collapsed)
  ;                      elaborated
  ;                      (eval collapsed)
  ;                      eval-result)
  ;              (and (equal? (syntax-object->datum collapsed) elaborated)
  ;                   (equal? (eval collapsed) eval-result))))
  ;          (list (list #'(let ([a 3] [b 9]) (+ a b)) '(let-values ([(a) (#%datum . 3)] [(b) (#%datum . 9)]) (#%app (#%top . +) a b)) 12)
  ;                (list #'(let* ([a 9] [b a] [c b]) c) '(let*-values ([(a) (#%datum . 9)] [(b) a] [(c) b]) c) 9)
  ;                (list #'(let ([a 3] [b 9]) (let ([b 14]) b)) '(let*-values ([(a) (#%datum . 3)] [(b) (#%datum . 9)] [(b) (#%datum . 14)]) b) 14)))
                                                                                                
                           ;                      ;                                ;            
 ;                         ;                      ;                                   ;         
;;;;  ;;;   ; ;;;          ;   ;;;  ;   ;   ;;;   ;      ; ;;  ;;;  ;   ;   ; ; ;; ; ;;;;  ;;;  
 ;   ;   ;  ;;   ;         ;  ;   ; ;   ;  ;   ;  ;      ;;   ;   ; ;   ;   ; ;;   ;  ;   ;   ; 
 ;   ;   ;  ;    ;         ;  ;   ;  ; ;   ;   ;  ;      ;    ;   ;  ; ; ; ;  ;    ;  ;   ;   ; 
 ;   ;   ;  ;    ;  ;;;;;  ;  ;;;;;  ; ;   ;;;;;  ;      ;    ;;;;;  ; ; ; ;  ;    ;  ;   ;;;;; 
 ;   ;   ;  ;    ;         ;  ;      ; ;   ;      ;      ;    ;      ; ; ; ;  ;    ;  ;   ;     
 ;   ;   ;  ;;   ;         ;  ;      ;;    ;      ;      ;    ;      ; ; ; ;  ;    ;  ;   ;     
  ;;  ;;;   ; ;;;          ;   ;;;;   ;     ;;;;  ;      ;     ;;;;   ;   ;   ;    ;   ;;  ;;;; 
            ;                                                                                   
            ;                                                                                   
                                                                                                

  
  ; top-level-rewrite : (SYNTAX-OBJECT -> SYNTAX-OBJECT)
  
  ; top-level-rewrite performs several tasks; it labels variables with their types (let-bound, lambda-bound, or non-lexical),
  ; it flags if's which could come from cond's, it labels the begins in conds with 'stepper-skip annotations
  
  ; label-var-types returns a syntax object which is identical to the original except that the variable references are labeled
  ; with the syntax-property 'stepper-binding-type, which is set to either let-bound, lambda-bound, or non-lexical.
  
  (define (top-level-rewrite stx)
    (let loop ([stx stx] [let-bound-bindings null] [cond-test (lambda (x) #f)] [and/or-test (lambda (x) #f)])
      (let* ([recur-regular 
              (lambda (stx)
                (loop stx let-bound-bindings (lambda (x) #f) (lambda (x) #f)))]
             [recur-with-bindings
              (lambda (exp vars)
                (loop exp (append vars let-bound-bindings) (lambda (x) #f) (lambda (x) #f)))]
             [recur-in-cond
              (lambda (stx new-cond-test)
                (loop stx let-bound-bindings new-cond-test (lambda (x) #f)))]
             [recur-in-and/or
              (lambda (stx new-and/or-test new-bindings)
                (loop stx (append new-bindings let-bound-bindings) (lambda (x) #f) new-and/or-test))]
             [do-let/rec
              (lambda (stx rec?)
                (with-syntax ([(label ((vars rhs) ...) . bodies) stx])
                  (let* ([vars-list (foldl (lambda (a b) (append b a)) null (map syntax->list (syntax->list (syntax (vars ...)))))]
                         [labelled-vars-list (map (lambda (var-list) (map (lambda (exp) (recur-with-bindings exp (syntax->list var-list)))
                                                                          vars-list))
                                                  (syntax->list (syntax (vars ...))))]
                         [rhs-list (if rec?
                                       (recur-with-bindings vars-list (syntax->list (syntax (rhs ...))))
                                       (map recur-regular (syntax->list (syntax (rhs ...)))))]
                         [new-bodies (map (lambda (exp) (recur-with-bindings exp vars-list)) (syntax->list (syntax bodies)))]
                         [new-bindings (map list labelled-vars-list rhs-list)])
                    (datum->syntax-object stx `(,(syntax label) ,new-bindings ,@new-bodies)))))]
             [do-and/or
              ; NOTE: I maintain my invariants in this section through foreknowledge of the shape of 
              ; the and/or macro. Therefore, this code is fragile.
              (lambda (new-and/or-test stx tag)
                (kernel:kernel-syntax-case stx #f
                  [(let-values [((test-var) test-exp)] (if if-test then else))
                   (let* ([new-test (recur-with-bindings (syntax if-test) (list (syntax test-var)))]
                          [new-then-else (case tag
                                           ((comes-from-and) (list (recur-in-and/or (syntax then) new-and/or-test (list (syntax test-var)))
                                                                   (recur-with-bindings (syntax else) (list (syntax test-var)))))
                                           ((comes-from-or) (list (recur-with-bindings (syntax then) (list (syntax test-var)))
                                                                  (recur-in-and/or (syntax else) new-and/or-test (list (syntax test-var))))))]
                          [new-if (syntax-property (rebuild-stx `(if ,new-test
                                                                     ,@new-then-else)
                                                                stx)
                                                  'stepper-hint
                                                  tag)])
                     (syntax-property (rebuild-stx `(let-values ([(,(syntax-property (recur-regular (syntax test-var))
                                                                                    'stepper-binding-type
                                                                                    'let-bound))
                                                                  ,(recur-regular (syntax test-exp))])
                                                      ,new-if)
                                                   stx)
                                      'stepper-hint
                                      tag))]))])
        (kernel:kernel-syntax-case stx #f

          ; and/or :
          [(let-values x ...)
           (let ([origin (syntax-property stx 'origin)])
             (and origin (pair? origin) (pair? (cdr origin)) (or (eq? (syntax-e (cadr origin)) 'or)
                                                                 (eq? (syntax-e (cadr origin)) 'and))))
           (let* ([origin-tag (syntax-e (cadr (syntax-property stx 'origin)))]
                  [tag (cond [(eq? origin-tag 'or) 'comes-from-or]
                            [(eq? origin-tag 'and) 'comes-from-and])])
             (do-and/or (lambda (test-stx) (and (eq? (syntax-source stx) (syntax-source test-stx))
                                                (eq? (syntax-position stx) (syntax-position test-stx))
                                                tag)) 
                        stx
                        tag))]
          [(let-values x ...)
           (and/or-test stx)
           (do-and/or and/or-test stx (and/or-test stx))]
          
          ; cond :
          [(if test (begin then) else-stx)
           (let ([origin (syntax-property stx 'origin)]
                 [rebuild-if
                  (lambda (new-cond-test)
                    (let* ([new-then (syntax-property (recur-regular (syntax (begin then)))
                                                            'stepper-skipto
                                                            (list syntax-e cdr car))])
                      (syntax-property
                       (rebuild-stx `(if ,(recur-regular (syntax test))
                                         ,new-then
                                         ,(recur-in-cond (syntax else-stx) new-cond-test))
                                    stx)
                       'stepper-hint
                       'comes-from-cond)))])
             (cond [(cond-test stx)
                    (rebuild-if cond-test)]
                   [(and origin (pair? origin) (eq? (syntax-e (car origin)) 'cond))
                    (rebuild-if (lambda (test-stx) 
                                  (and (eq? (syntax-source stx) (syntax-source test-stx))
                                       (eq? (syntax-position stx) (syntax-position test-stx)))))]
                   [else
                    (rebuild-stx `(if ,@(map recur-regular (list (syntax test) (syntax (begin then)) (syntax else-stx)))) stx)]))]
          [(begin body) ; else clauses of conds
           (cond-test stx)
           (let ([new-body (syntax-property (recur-regular (syntax body)) 'stepper-else #t)])
             (syntax-property (rebuild-stx `(begin ,new-body) stx) 'stepper-skipto (list syntax-e cdr car)))]
          
          
          ; let/letrec :
          [(let-values x ...) (do-let/rec stx #f)]
          [(letrec-values x ...) (do-let/rec stx #t)]
          [var
           (identifier? (syntax var))
           (if (eq? (identifier-binding (syntax var)) 'lexical)
               (if (ormap (lambda (binding)
                            (bound-identifier=? binding (syntax var)))
                          let-bound-bindings)
                   (syntax-property (syntax var) 'stepper-binding-type 'let-bound)
                   (syntax-property (syntax var) 'stepper-binding-type 'lambda-bound))
               (syntax-property (syntax var) 'stepper-binding-type 'non-lexical))]
          [stx
           (let ([content (syntax-e (syntax stx))])
             (if (pair? content)
                 (datum->syntax-object (syntax stx)
                                       (syntax-pair-map content recur-regular)
                                       (syntax stx)
                                       (syntax stx))
                 content))]))))
  
;  (syntax-case (label-var-types (expand #'(+ a 3))) (#%app #%top + #%datum) 
;    [(#%app (#%top . +) (#%top . a-var) (#%datum . 3))
;     (test 'non-lexical syntax-property (syntax a-var) 'stepper-binding-type)])
;  
;  (syntax-case (label-var-types (expand #'(let ([a a]) (+ a b)))) (let-values + #%app #%top)
;    [(let-values ([(a-var-0) (#%top . a-var-1)]) (#%app (#%top . +) a-var-2 (#%top . b-var)))
;     (begin
;       (test 'let-bound syntax-property (syntax a-var-0) 'stepper-binding-type)
;       (test 'non-lexical syntax-property (syntax a-var-1) 'stepper-binding-type)
;       (test 'let-bound syntax-property (syntax a-var-2) 'stepper-binding-type))])
;  
;  (syntax-case (label-var-types (expand #'(letrec ([a a]) (lambda (a) a)))) (letrec-values lambda)
;    [(letrec-values ([a-0 a-var-0])
;       (lambda (a-1) a-var-1))
;     (begin
;       (test 'let-bound syntax-property (syntax a-var-0) 'stepper-binding-type)
;       (test 'lambda-bound syntax-property (syntax a-1) 'stepper-binding-type)
;       (test 'lambda-bound syntax-property (syntax a-var-1) 'stepper-binding-type))])
;
;  (syntax-case (label-var-types (expand #'(let ([a 3]) (let ([b 4]) a)))) (let-values #%datum)
;    [(let-values ([(a-var-0) (#%datum . 3)])
;       (let-values ([(b-var-0) (#%datum . 4)])
;         a-var-1))
;     (begin
;       (test 'let-bound syntax-property (syntax a-var-1) 'stepper-binding-type))])       
                                                      

                                                 
                                                 
                                                 
   ;                                               
  ; ;                         ;          ;         
  ; ;    ; ;;   ; ;;    ;;;  ;;;;  ;;;  ;;;;  ;;;  
  ; ;    ;;  ;  ;;  ;  ;   ;  ;   ;   ;  ;   ;   ; 
 ;   ;   ;   ;  ;   ;  ;   ;  ;       ;  ;   ;   ; 
 ;;;;;   ;   ;  ;   ;  ;   ;  ;    ;;;;  ;   ;;;;; 
 ;   ;   ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;     
;     ;  ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;     
;     ;  ;   ;  ;   ;   ;;;    ;;  ;;;;;  ;;  ;;;; 
                                                   
                                                   
                                                   
  
;  oh-say-can-you-see,by-the-dawn's-early-light,what-so-proudly-we-hailed,at-the-twilight's-last-gle
;  a m i n g . W h o s e b r o a d s t r i                                                         p
;  pe s a n d b r i g h t s t a r s , t hrough-the-perilous-night,o'er-the-ramparts-we-watched,were-
;  s o g a l l a n t l y s t r e a m i n g                                                         .
;  an d t h e r o c k e t ' s r e d g l are,the-bombs-bursting-in-air,gave-proof-through-the-night,,
;  t h a t o u r f l a g w a s s t i l l t                                                         h
;  er e . O h s a y , d o e s t h a t s tar-spangled-banner-yet-wave,o'er-the-land-of-the-free,and-t
;  h e h o m e o f t h e b r a v e ? . . .                                                         .
;  .. . . . . . . . . . . . . . . . . . ............................................................
;  . . . . . . . . . . . . . . . . . . . .                                                         .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;  .                                                                                               .
;  .................................................................................................
;
  
  
  ; annotate takes 
  ; a) a list of zodiac:read expressions,
  ; b) a list of syntax expressions
  ; c) a list of previously-defined variables, 
  ; d) a break routine to be called at breakpoints, and
  ; e) a symbol which indicates how to annotate the source.  Currently, there are three
  ;    styles: 'cheap-wrap, 'ankle-wrap, and 'foot-wrap.
  ; f) optionally, a list of symbols which modifies the annotation.  Currently, valid 
  ;    choices include: 'no-closure-capturing, which eliminates closure shadowing, and
  ;    'no-temps-for-varrefs, which prevents rewriting (to capture intermediate values)
  ;    of applications and ifs which consist
  ;    only of varrefs.  This one might go away later, or be toggled into a "no-opt" flag.
  ;
  
  (define (annotate expr annotate-environment break wrap-style . wrap-opts-list)
    (local
	((define cheap-wrap? (eq? wrap-style 'cheap-wrap))
         (define ankle-wrap? (eq? wrap-style 'ankle-wrap))
         (define foot-wrap? (eq? wrap-style 'foot-wrap))
         (define wrap-opts (cond [(null? wrap-opts-list) null]
                                 [(not (= (length wrap-opts-list) 1))
                                  (error 'annotate "wrong number of arguments.")]
                                 [(not (and (list? (car wrap-opts-list))
                                            (andmap symbol? (car wrap-opts-list))))
                                  (error 'annotate "wrap-opts-list argument must be a list of symbols. Given: ~a~n"
                                         (car wrap-opts-list))]
                                 [else (car wrap-opts-list)]))

         ; potential optimization: remove the var-args where it's not needed:
         (define (make-break kind)
           (lambda returned-value-list
             (break (current-continuation-marks) debug-key kind returned-value-list)))

         (define normal-break
           (make-break 'normal-break))

         (define result-exp-break
           (make-break 'result-exp-break))
         
         (define result-value-break
           (make-break 'result-value-break))
         
         (define input-struct-proc-names (car annotate-environment))
         (define input-user-defined-names (cadr annotate-environment))
         (define binding-index (caddr annotate-environment))
         
         (define (binding-indexer)
           (let ([index binding-index])
             (set! binding-index (+ binding-index 1))
             index))
         
         ; wrap creates the w-c-m expression.

         
         
         ; here are the possible configurations of wcm's, pre-breaks, and breaks (not including late-let & double-breaks):
  
         ; (for full-on stepper)
         ; wcm, result-break, normal-break
         ; wcm, normal-break
         
         ; simple-wcm-wrap : just put the w-c-m on the expression
         (define (simple-wcm-wrap debug-info expr)
           (d->so `(with-continuation-mark ,debug-key ,debug-info ,expr)))

         ; wcm-pre-break-wrap : call simple-wcm-wrap with a pre-break on the expr
         (define (wcm-pre-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info (d->so `(begin (,result-exp-break) ,expr))))
         
         (define (break-wrap expr)
           (d->so `(begin (,normal-break) ,expr)))
         
         ; turning off both kinds of let-wrapping :
         
         (define (double-break-wrap expr)
           expr)
           ;(d->so `(begin (,(make-break 'double-break)) ,expr)))
         
         (define (late-let-break-wrap var-names lifted-gensyms expr)
           expr)
           ;(let* ([interlaced (apply append (map list var-names lifted-gensyms))])
           ;      (d->so `(begin (,(make-break 'late-let-break) ,@interlaced) ,expr))))
         
         (define (return-value-wrap expr)
           (d->so
            `(let* ([result ,expr])
               (,result-value-break result)
               result)))

;  For Multiple Values:         
;           `(#%call-with-values
;             (#%lambda ()
;              expr)
;             (#%lambda result-values
;              (,(make-break 'result-break) result-values)
;              (#%apply #%values result-values))))

         
         (define (defined-names expr)
           (syntax-case expr (define-values)
                   [(define-values vars body)
                    (syntax->list (syntax vars))]
                   [else
                    null]))
         
         (define (struct-procs-defined expr)
           (let ([origin (syntax-property expr 'origin)])
             (if (and origin
                      (ormap (lambda (origin-entry)
                               (eq? (syntax-e origin-entry) 'define-struct))
                             origin))
                 (defined-names expr)
                 null)))
         
         (define struct-proc-names (append (struct-procs-defined expr)
                                           input-struct-proc-names))
         
         (define user-defined-names (append (defined-names expr)
                                       input-user-defined-names))
         
         (define (non-annotated-proc? varref)
           (or (not (ormap (lambda (id)
                             (bound-identifier=? id varref))
                           user-defined-names))
               (ormap (lambda (id)
                        (bound-identifier=? id varref))
                      struct-proc-names)))
         
         (define (top-level-annotate/inner expr)
           (annotate/inner expr 'all #f #t #f))
         
         ; annotate/inner takes 
         ; a) an expression to annotate
         ; b) a list of all bindings which this expression is tail w.r.t. 
         ;    or 'all to indicate that this expression is tail w.r.t. _all_ bindings.
         ; d) a boolean indicating whether this expression will be the r.h.s. of a reduction
         ;    (and therefore should be broken before)
         ; e) a boolean indicating whether this expression is top-level (and therefore should
         ;    not be wrapped, if a begin).
         ; g) information about the binding name of the given expression.  This is used 
         ;    to associate a name with a closure mark (though this may now be redundant)
         ;    and to set up a (let ([x y]) x) so that mzscheme gets the right inferred-name
         ;    for closures

         ; it returns (as a 2vals)
         ; a) an annotated s-expression
         ; b) a list of varrefs for the variables which occur free in the expression
         ;
	 ;(syntax-object BINDING-SET bool bool (union #f symbol (list binding symbol)) -> 
         ;          sexp (list-of z:varref))
         

                                                                                     
                                                                                     
                                                                                     
                                                      ;  ;                           
                             ;          ;             ;                              
  ;;;   ; ;;   ; ;;    ;;;  ;;;;  ;;;  ;;;;  ;;;     ;   ;  ; ;;   ; ;;    ;;;   ; ;;
 ;   ;  ;;  ;  ;;  ;  ;   ;  ;   ;   ;  ;   ;   ;    ;   ;  ;;  ;  ;;  ;  ;   ;  ;;  
     ;  ;   ;  ;   ;  ;   ;  ;       ;  ;   ;   ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   
  ;;;;  ;   ;  ;   ;  ;   ;  ;    ;;;;  ;   ;;;;;   ;    ;  ;   ;  ;   ;  ;;;;;  ;   
 ;   ;  ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;       ;    ;  ;   ;  ;   ;  ;      ;   
 ;   ;  ;   ;  ;   ;  ;   ;  ;   ;   ;  ;   ;       ;    ;  ;   ;  ;   ;  ;      ;   
  ;;;;; ;   ;  ;   ;   ;;;    ;;  ;;;;;  ;;  ;;;;   ;    ;  ;   ;  ;   ;   ;;;;  ;   
                                                   ;                                 
                                                   ;                                 
                                                                                     
	 (define annotate/inner 
           (checked-lambda ((expr SYNTAX-OBJECT) (tail-bound BINDING-SET) (pre-break? BOOLEAN) (top-level? BOOLEAN) 
                            procedure-name-info)
                           
           (if (syntax-property expr 'stepper-skipto)
               (let ([free-vars-captured #f]) ; this will be set!'ed
                 ; WARNING! I depend on the order of evaluation in application arguments here:
                 (2vals (skipto-annotate
                         (syntax-property expr 'stepper-skipto) 
                         expr 
                         (lambda (subterm)
                           (let*-2vals ([(stx free-vars) (annotate/inner subterm tail-bound pre-break? top-level? procedure-name-info)])
                                       (set! free-vars-captured free-vars)
                                       stx)))
                        free-vars-captured))
                 
               
               (let* ([d->so/user (lambda (stx) (datum->syntax-object #'here stx expr))]
                      [tail-recur (lambda (expr) (annotate/inner expr tail-bound #t #f procedure-name-info))]
                      [define-values-recur (lambda (expr name) 
                                             (annotate/inner expr tail-bound #f #f name))]
                      [non-tail-recur (lambda (expr) (annotate/inner expr null #f #f #f))]
                      [result-recur (lambda (expr) (annotate/inner expr null #f #f procedure-name-info))]
                      [set!-rhs-recur (lambda (expr name) (annotate/inner expr null #f #f name))]
                      [let-rhs-recur (lambda (expr binding-names dyn-index-syms bindings)
                                       (let* ([proc-name-info 
                                               (if (not (null? binding-names))
                                                   (list (car binding-names) (car dyn-index-syms))
                                                   #f)])
                                         (annotate/inner expr null #f #f proc-name-info)))]
                      [lambda-body-recur (lambda (expr) (annotate/inner expr 'all #t #f #f))]
                      ; note: no pre-break for the body of a let; it's handled by the break for the
                      ; let itself.
                      [let-body-recur (lambda (bindings)
                                        (lambda (expr) 
                                          (annotate/inner expr (binding-set-union (list tail-bound bindings)) #f 
                                                          #f procedure-name-info)))]
                      [cheap-wrap-recur (lambda (expr) (let-values ([(ann _) (tail-recur expr)]) ann))]
                      [no-enclosing-recur (lambda (expr) (annotate/inner expr 'all #f #f #f))]
                      [make-debug-info-normal (lambda (free-bindings)
                                                (make-debug-info expr tail-bound free-bindings 'none foot-wrap?))]
                      [make-debug-info-app (lambda (tail-bound free-bindings label)
                                             (make-debug-info expr tail-bound free-bindings label foot-wrap?))]
                      [make-debug-info-let (lambda (free-bindings binding-list let-counter)
                                             (make-debug-info expr 
                                                              (binding-set-union (list tail-bound 
                                                                                       binding-list
                                                                                       (list let-counter)))
                                                              (varref-set-union (list free-bindings 
                                                                                      binding-list
                                                                                      (list let-counter))) ; NB using bindings as varrefs
                                                              'let-body
                                                              foot-wrap?))]
                      [wcm-wrap (if pre-break?
                                    wcm-pre-break-wrap
                                    simple-wcm-wrap)]
                      [wcm-break-wrap (lambda (debug-info expr)
                                        (wcm-wrap debug-info (break-wrap expr)))]
                      [expr-cheap-wrap (lambda (annotated) (cheap-wrap expr annotated))]
                      [ankle-wcm-wrap (lambda (expr free-bindings)
                                        (simple-wcm-wrap (make-debug-info-normal free-bindings) expr))]
                      [appropriate-wrap (lambda (annotated free-bindings)
                                          (cond [cheap-wrap? (cheap-wrap expr annotated)]
                                                [ankle-wrap? (ankle-wcm-wrap annotated free-bindings)]
                                                [else (error 'appropriate-wrap "wrap is neither cheap nor ankle")]))]
                      [inferred-name-patch (lambda (annotated)
                                             (if (and procedure-name-info (not (memq 'no-closure-capturing wrap-opts)))
                                                 (let ([name (ccond [(symbol? procedure-name-info) procedure-name-info]
                                                                    [(and (list? procedure-name-info)
                                                                          (= (length procedure-name-info) 2))
                                                                     (car procedure-name-info)])])
                                                   (d->so `(let* ([,name ,annotated]) ,name)))
                                                 annotated))]
                      
                      [lambda-clause-abstraction 
                       (lambda (clause)
                         (with-syntax ([(args-stx . bodies) clause])
                           (let*-2vals ([args (syntax->ilist (syntax args-stx))]
                                        [(annotated-body free-varrefs)
                                         (if (= (length (syntax->list (syntax bodies))) 1)
                                             (lambda-body-recur (car (syntax->list (syntax bodies))))
                                             (lambda-body-recur (syntax (begin . bodies))))]
                                        [tagged-body (syntax-property annotated-body 'stepper-info 'lambda-body-begin)]
                                        [new-free-varrefs (varref-set-remove-bindings free-varrefs
                                                                                      (ilist-flatten args))])
                                       (2vals (datum->syntax-object #'here (list args tagged-body) clause) new-free-varrefs))))]
                      
                      [outer-lambda-abstraction
                       (lambda (annotated-lambda free-varrefs)
                         (if cheap-wrap?
                             (2vals annotated-lambda free-varrefs)
                             (let*-2vals
                              ([closure-info (make-debug-info-app 'all free-varrefs 'none)]
                               [closure-storing-proc
                                (lambda (closure debug-info . extra)
                                  (closure-table-put! closure (make-closure-record 
                                                               #f
                                                               debug-info
                                                               #f
                                                               (if (not (null? extra))
                                                                   (car extra)
                                                                   #f)))
                                  closure)]
                               [inferred-name-lambda
                                (cond [(symbol? procedure-name-info)
                                       (syntax-property annotated-lambda 'inferred-name procedure-name-info)]
                                      [(pair? procedure-name-info)
                                       (syntax-property annotated-lambda 'inferred-name (car procedure-name-info))]
                                      [else
                                       annotated-lambda])]
                               [captured
                                (if (memq 'no-closure-capturing wrap-opts)
                                    inferred-name-lambda
                                    (cond [(symbol? procedure-name-info)
                                           (d->so `(,closure-storing-proc ,inferred-name-lambda ,closure-info))]
                                          [(pair? procedure-name-info)
                                           (if foot-wrap?
                                               (d->so `(,closure-storing-proc ,inferred-name-lambda ,closure-info 
                                                        ,(cadr procedure-name-info)))
                                               (d->so `(,closure-storing-proc ,inferred-name-lambda ,closure-info 
                                                        #f)))]
                                          [else
                                           (d->so `(,closure-storing-proc ,inferred-name-lambda ,closure-info))]))])
                              
                              (2vals
                               (ccond [foot-wrap? 
                                       (wcm-wrap (make-debug-info-normal free-varrefs)
                                                 captured)]
                                      [ankle-wrap? 
                                       captured]) ; no wcm is needed because evaluation of closures cannot cause exceptions.
                               free-varrefs))))]
                      
                      ; The let transformation is complicated.
                      ; here's a sample transformation (not including 'break's):
                      ;(let-values ([(a b c) e1] [(d e) e2]) e3)
                      ;
                      ;turns into
                      ;
                      ;(let-values ([(a b c d e lifter-a-1 lifter-b-2 lifter-c-3 lifter-d-4 lifter-e-5 let-counter)
                      ;              (values *unevaluated* *unevaluated* *unevaluated* *unevaluated* *unevaluated*
                      ;                      (<dynamic-counter-call>) (<dynamic-counter-call>) (<dynamic-counter-call>) 
                      ;                      (<dynamic-counter-call>) (<dynamic-counter-call>) 0)])
                      ;  (with-continuation-mark 
                      ;   key huge-value
                      ;   (begin
                      ;     (set!-values (a b c) e1)
                      ;     (set! let-counter 1)
                      ;     (set!-values (d e) e2)
                      ;     (set! let-counter 2)
                      ;     e3)))
                      ;
                      ; note that this elaboration looks exactly like the one for letrec, and that's
                      ; okay, becuase expand guarantees that reordering them will not cause capture.
                      ; this is because a bound variable answers is considered bound by a binding only when
                      ; the pair answers true to bound-identifier=?, which is determined during (the first)
                      ; expand.
                      
                      ; another irritating point: the mark and the break that must go immediately 
                      ; around the body.  Irritating because they will be instantly replaced by
                      ; the mark and the break produced by the annotated body itself. However, 
                      ; they're necessary, because the body may not contain free references to 
                      ; all of the variables defined in the let, and thus their values are not 
                      ; known otherwise.  
                      ; whoops! hold the phone.  I think I can get away with a break before, and
                      ; a mark after, so only one of each.  groovy, eh?
                      
                      [let-abstraction
                       (lambda (stx output-identifier make-init-list)
                         (with-syntax ([(_ ([var val] ...) . bodies) stx]
                                       [(_a (binding ...) . _b) stx])
                           (let*-2vals
                            ([binding-sets (map syntax->list (syntax->list (syntax (var ...))))]
                             [binding-name-sets (mapmap syntax-e binding-sets)]
                             [binding-list (foldl append null binding-sets)]
                             [vals (syntax->list (syntax (val ...)))]
                             [lifted-var-sets (map (lambda (x) (map get-lifted-var x)) binding-sets)]
                             [lifted-vars (apply append lifted-var-sets)]
                             [(annotated-vals free-varref-sets-vals)
                              (2vals-map let-rhs-recur vals binding-name-sets lifted-var-sets binding-sets)]
                             [(annotated-body free-varrefs-body)
                              ((let-body-recur binding-list) 
                               (if (= (length (syntax->list (syntax bodies))) 1)
                                   (car (syntax->list (syntax bodies)))
                                   (syntax (begin . bodies))))]
                             [tagged-body (syntax-property annotated-body 'stepper-info 'let-body-begin)]
                             [free-varrefs (varref-set-remove-bindings 
                                            (varref-set-union (cons free-varrefs-body
                                                                    free-varref-sets-vals)) 
                                            binding-list)])
                            
                            (ccond [cheap-wrap?
                                    (let* ([bindings
                                            (map (lambda (binding-loc bindings val)
                                                   (datum->syntax-object #'here `(,bindings ,val) binding-loc))
                                                 (syntax->list (syntax (binding ...)))
                                                 binding-sets
                                                 annotated-vals)]
                                           [annotated
                                            (d->so/user `(,output-identifier ,bindings ,tagged-body))])
                                      (2vals (appropriate-wrap annotated free-varrefs) free-varrefs))]
                                   [(or ankle-wrap? foot-wrap?)
                                    (let* ([unevaluated-list (make-init-list binding-list)]
                                           [outer-initialization
                                            (if ankle-wrap?
                                                (d->so `((,binding-list (values ,@unevaluated-list))))
                                                (d->so `([,(append lifted-vars binding-list (list let-counter))
                                                          (values ,@(append (map (lambda (binding)
                                                                                   (d->so `(,binding-indexer))) 
                                                                                 binding-list)
                                                                            unevaluated-list
                                                                            (list 0)))])))]
                                           [counter-clauses (build-list 
                                                             (length binding-sets)
                                                             (lambda (num)
                                                               (d->so `(set! ,let-counter ,(+ num 1)))))]
                                           [set!-clauses
                                            (map (lambda (binding-set val)
                                                   (d->so `(set!-values ,binding-set ,val)))
                                                 binding-sets
                                                 annotated-vals)]
                                           [interlaced-clauses
                                            (foldl (lambda (a b) (append b a)) null 
                                                   (zip set!-clauses counter-clauses))] 
                                           ; time to work from the inside out again
                                           ; without renaming, this would all be much much simpler.
                                           [middle-begin
                                            (double-break-wrap (d->so `(begin ,@interlaced-clauses 
                                                                              ,(late-let-break-wrap binding-list
                                                                                                    lifted-vars
                                                                                                    tagged-body))))]
                                           [wrapped-begin (wcm-wrap (make-debug-info-let free-varrefs
                                                                                         binding-list
                                                                                         let-counter) 
                                                                    middle-begin)]
                                           [whole-thing (d->so/user `(,output-identifier ,outer-initialization ,wrapped-begin))])
                                      (2vals whole-thing free-varrefs))]))))]
                      
                      )
                 
                 ; find the source expression and associate it with the parsed expression
                 
                 ;             (when (and red-exprs foot-wrap?)
                 ;               (set-expr-read! expr (find-read-expr expr))) 
                 
                 
                 (kernel:kernel-syntax-case expr #f
                   
                   [(lambda . clause)
                    (let*-2vals ([(annotated-clause free-varrefs)
                                  (lambda-clause-abstraction (syntax clause))]
                                 [annotated-lambda
                                  (with-syntax ([annotated-clause annotated-clause])
                                    (syntax/loc expr (lambda . annotated-clause)))])
                                (outer-lambda-abstraction annotated-lambda free-varrefs))]
                   
                   [(case-lambda . clauses)
                    (let*-2vals ([(annotated-cases free-varrefs-cases)
                                  (2vals-map lambda-clause-abstraction (syntax->list (syntax clauses)))]
                                 [annotated-case-lambda (with-syntax ([annotated-cases annotated-cases])
                                                          (syntax/loc expr (case-lambda . annotated-cases)))]
                                 [free-varrefs (varref-set-union free-varrefs-cases)])
                                (outer-lambda-abstraction annotated-case-lambda free-varrefs))]
                   
                   ; for if's, we assume (for foot-wrap) that the "test" is a varref, and thus the if does not
                   ; need to be rewritten to move the non-tail part outside of the source break
                   ; (this is true in beginner, intermediate, & advanced)
                   
                   [(if test then else)
                    (let*-2vals
                     ([(annotated-test free-varrefs-test) 
                       (non-tail-recur (syntax test))]
                      [(annotated-then free-varrefs-then) 
                       (tail-recur (syntax then))]
                      [(annotated-else free-varrefs-else) 
                       (tail-recur (syntax else))]
                      [free-varrefs (varref-set-union (list free-varrefs-test 
                                                            free-varrefs-then 
                                                            free-varrefs-else))]
                      [annotated-if
                       (with-syntax ([test-stx annotated-test]
                                     [then-stx annotated-then]
                                     [else-stx annotated-else]
                                     [break-stx normal-break])
                         (syntax/loc expr (let ([test-var test-stx])
                                            (begin (break-stx) (if test-var then-stx else-stx)))))])
                     (2vals
                      (if foot-wrap?
                          (wcm-wrap (make-debug-info-normal free-varrefs) annotated-if)
                          (appropriate-wrap annotated-if free-varrefs))
                      free-varrefs))]
                   
                   ; yecch: should abstract over if with & without else clauses
                   
                   [(if test then)
                    (let*-2vals
                     ([(annotated-test free-varrefs-test) 
                       (non-tail-recur (syntax test))]
                      [(annotated-then free-varrefs-then) 
                       (tail-recur (syntax then))]
                      [free-varrefs (varref-set-union (list free-varrefs-test 
                                                            free-varrefs-then))]
                      [annotated-if
                       (with-syntax ([test annotated-test]
                                     [then annotated-then])
                         (syntax/loc expr (if test then)))])
                     (2vals
                      (if foot-wrap?
                          (wcm-break-wrap (make-debug-info-normal free-varrefs) annotated-if)
                          (appropriate-wrap annotated-if free-varrefs))
                      free-varrefs))]
                   
                   [(begin . bodies-stx)
                    (if top-level? 
                        (let*-2vals
                         ([(annotated-bodies free-varref-sets)
                           (2vals-map (lambda (expr)
                                        (top-level-annotate/inner expr)) 
                                      (syntax->list (syntax bodies-stx)))])
                         (2vals (d->so/user `(begin ,@annotated-bodies))
                                (varref-set-union free-varref-sets)))
                        (let*-2vals 
                         ([bodies (syntax->list (syntax bodies-stx))]
                          [(all-but-last-body last-body-list) 
                           (list-partition bodies (- (length bodies) 1))]
                          [last-body (car last-body-list)]
                          [(annotated-a free-varrefs-a)
                           (2vals-map non-tail-recur all-but-last-body)]
                          [(annotated-final free-varrefs-final)
                           (tail-recur last-body)]
                          [free-varrefs (varref-set-union (cons free-varrefs-final free-varrefs-a))]
                          [debug-info (make-debug-info-normal free-varrefs)]
                          [annotated (d->so/user `(begin ,@(append annotated-a (list annotated-final))))])
                         (2vals (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-varrefs)]
                                       [foot-wrap? (wcm-wrap debug-info annotated)])
                                free-varrefs)))]
                   
                   [(begin0 . bodies-stx)
                    (let*-2vals
                     ([bodies (syntax->list (syntax bodies-stx))]
                      [(annotated-first free-varrefs-first)
                       (result-recur (car bodies))]
                      [(annotated-bodies free-varref-sets)
                       (2vals-map non-tail-recur (cdr bodies))]
                      [free-varrefs (varref-set-union (cons free-varrefs-first free-varref-sets))]
                      [debug-info (make-debug-info-normal free-varrefs)]
                      [annotated (d->so/user `(begin0 ,annotated-first ,@annotated-bodies))])
                     (2vals (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-varrefs)]
                                   [foot-wrap?
                                    (wcm-wrap debug-info annotated)])
                            free-varrefs))]
                   
                   [(let-values . _)
                    (let*-2vals ([collapsed (collapse-let-values expr)])
                                (let-abstraction collapsed 
                                                 'let*-values
                                                 (lambda (bindings)
                                                   (map (lambda (_) *unevaluated*) bindings))))]
                   
                   [(letrec-values . _)
                    (let-abstraction expr 
                                     'letrec-values
                                     (lambda (bindings) (map d->so bindings)))]
                   
                   [(set! var val)
                    (let*-2vals
                     ([(annotated-val val-free-varrefs)
                       (set!-rhs-recur (syntax val) (syntax-case (syntax var) (#%top)
                                                      [(#%top . real-var) (syntax-e (syntax real-var))]
                                                      [else (syntax var)]))]
                      [free-varrefs (varref-set-union (list (list (syntax var))
                                                            val-free-varrefs))]
                      [debug-info (make-debug-info-normal free-varrefs)]
                      [annotated (d->so/user `(set! ,(syntax var) ,annotated-val))])
                     (2vals (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-varrefs)]
                                   [foot-wrap?
                                    (wcm-wrap (make-debug-info-normal free-varrefs) annotated)])
                            free-varrefs))]
                   
                   
                   [(quote _)
                    (2vals
                     (if (or cheap-wrap? ankle-wrap?)
                         expr
                         (wcm-wrap (make-debug-info-normal null) expr))
                     null)]
                   
                   [(quote-syntax _)
                    (2vals
                     (if (or cheap-wrap? ankle-wrap?)
                         expr
                         (wcm-wrap (make-debug-info-normal null) expr))
                     null)]
                   
                   [(with-continuation-mark key mark body)
                    (let*-2vals ([(annotated-key free-varrefs-key)
                                  (non-tail-recur (syntax key))]
                                 [(annotated-mark free-varrefs-mark)
                                  (non-tail-recur (syntax mark))]
                                 [(annotated-body free-varrefs-body)
                                  (result-recur (syntax body))])
                                (ccond [(or cheap-wrap? ankle-wrap?)
                                        (let*-2vals
                                         ([free-varrefs (varref-set-union (list free-varrefs-key free-varrefs-mark free-varrefs-body))]
                                          [annotated (d->so/user `(with-continuation-mark ,annotated-key
                                                                                          ,annotated-mark
                                                                                          ,annotated-body))])
                                         (2vals (appropriate-wrap annotated free-varrefs) free-varrefs))]
                                       [foot-wrap?
                                        (error 'annotate/inner "this region of code is still under construction")
                                        
                                        ;                                       [annotated (d->so `(let-values ([key-temp ,*unevaluated*]
                                        ;                                             [mark-temp ,*unevaluated*])
                                        
                                        ]))]
                   
                   ;                                  [foot-wrap? 
                   ;                                   (wcm-wrap debug-info annotated)])
                   ;                           free-bindings))]
                   
                   ; the app form's elaboration looks like this, where M0 etc. stand for expressions, and t0 etc
                   ; are temp identifiers that do not occur in the program:
                   ; (M0 ...)
                   ;
                   ; goes to
                   ;
                   ;(let ([t0 *unevaluated*]
                   ;      ...)
                   ;  (with-continuation-mark
                   ;   debug-key
                   ;   huge-value
                   ;   (set! t0 M0)
                   ;   ...
                   ;   (with-continuation-mark
                   ;    debug-key
                   ;    much-smaller-value
                   ;    (t0 ...))))
                   ; 
                   ; 'break's are not illustrated.  An optimization is possible when all expressions M0 ... are
                   ; varrefs.  In particular (where v0 ... are varrefs):
                   ; (v0 ...)
                   ;
                   ; goes to
                   ; 
                   ; (with-continuation-mark
                   ;  debug-key
                   ;  debug-value
                   ;  (v0 ...))
                   ;
                   ; in other words, no real elaboration occurs. Note that this doesn't work as-is for the
                   ; stepper, because there's nowhere to hang the breakpoint; you want to see the break
                   ; occur after all vars have been evaluated.  I suppose you could do (wcm ... (begin v0 ... (v0 ...)))
                   ; where the second set are not annotated ... but stepper runtime is not at a premium.
                   
                   [(#%app . terms)
                    (let*-2vals
                     ([(annotated-terms free-varrefs-terms)
                       (2vals-map non-tail-recur (syntax->list (syntax terms)))]
                      [free-varrefs (varref-set-union free-varrefs-terms)]
                      [annotated (d->so/user annotated-terms)])
                     (2vals
                      (ccond [cheap-wrap? (appropriate-wrap annotated free-varrefs)]
                             [(or ankle-wrap? foot-wrap?)
                              (if (and ankle-wrap?
                                       (memq 'no-temps-for-varrefs wrap-opts)
                                       (andmap term-is-reduced
                                               (syntax->list (syntax terms))))
                                  
                                  ; this is the no-temps optimization:
                                  ; (won't work for stepper unless no reductions happen on the vars in the app
                                  ; oh! what if they're all lambda-bound vars? some other day, perhaps.
                                  
                                  (let ([debug-info (make-debug-info-app tail-bound free-varrefs 'called)])
                                    (wcm-break-wrap debug-info annotated-terms))
                                  
                                  (let* ([arg-temps (build-list (length annotated-terms) get-arg-var)]
                                         [tagged-arg-temps (map (lambda (var) (syntax-property var 'stepper-binding-type 'stepper-temp))
                                                                arg-temps)]
                                         [let-clauses (d->so `((,tagged-arg-temps 
                                                                (values ,@(map (lambda (_) *unevaluated*) tagged-arg-temps)))))]
                                         [set!-list (map (lambda (arg-symbol annotated-sub-expr)
                                                           (d->so `(set! ,arg-symbol ,annotated-sub-expr)))
                                                         tagged-arg-temps annotated-terms)]
                                         [new-tail-bound (binding-set-union (list tail-bound tagged-arg-temps))]
                                         [app-debug-info (make-debug-info-app new-tail-bound tagged-arg-temps 'called)]
                                         [final-app (break-wrap (simple-wcm-wrap 
                                                                 app-debug-info
                                                                 (if (syntax-case (car (syntax->list (syntax terms))) (#%top)
                                                                       [(#%top . var)
                                                                        (and foot-wrap? 
                                                                             (non-annotated-proc? (syntax var)))]
                                                                       [var
                                                                        (identifier? (syntax var)) ; guard
                                                                        (and (not (eq? (identifier-binding (syntax var)) 'lexical))
                                                                             foot-wrap?
                                                                             (non-annotated-proc? (syntax var)))]
                                                                       [else #f])
                                                                     (return-value-wrap (d->so/user arg-temps))
                                                                     (d->so/user tagged-arg-temps))))]
                                         [debug-info (make-debug-info-app new-tail-bound
                                                                          (varref-set-union (list free-varrefs tagged-arg-temps)) ; NB using bindings as vars
                                                                          'not-yet-called)]
                                         [let-body (wcm-wrap debug-info (d->so `(begin ,@set!-list ,final-app)))])
                                    (d->so `(let-values ,let-clauses ,let-body))))])
                      free-varrefs))]   
                   
                   [(#%datum . _)
                    (2vals
                     (if (or cheap-wrap? ankle-wrap?)
                         expr
                         (wcm-wrap (make-debug-info-normal null) expr))
                     null)]
                   
                   [(define-values vars-stx body)
                    (let*-2vals
                     ([vars (syntax->list (syntax vars-stx))]
                      [(annotated-val free-varrefs-val)
                       (define-values-recur (syntax body) (if (not (null? vars))
                                                              (syntax-e (car vars))
                                                              #f))])
                     (2vals
                      (d->so/user `(define-values ,(syntax vars-stx) ,annotated-val))
                      free-varrefs-val))]
                   
                   
                   [(#%top . var)
                    (let*-2vals ([var (syntax var)]
                                 [free-varrefs (list var)])
                                (2vals 
                                 (ccond [(or cheap-wrap? ankle-wrap?)
                                         (appropriate-wrap var free-varrefs)]
                                        [foot-wrap?
                                         (wcm-break-wrap (make-debug-info-normal free-varrefs)
                                                         (return-value-wrap var))])
                                 free-varrefs))]
                   
                   [var-stx
                    (identifier? (syntax var-stx))
                    (let*-2vals ([var (syntax var-stx)]
                                 [free-varrefs (list var)])
                                (2vals 
                                 (ccond [(or cheap-wrap? ankle-wrap?)
                                         (appropriate-wrap var free-varrefs)]
                                        [foot-wrap? 
                                         (case (syntax-property var 'stepper-binding-type)
                                           ((lambda-bound) (wcm-wrap (make-debug-info-normal free-varrefs) var))
                                           ((let-bound) (wcm-break-wrap (make-debug-info-normal free-varrefs) (return-value-wrap var)))
                                           ((non-lexical) (wcm-break-wrap (make-debug-info-normal free-varrefs)
                                                                          (return-value-wrap var))))])
                                 free-varrefs))]
                   
                   [else ; require, require-for-syntax, define-syntaxes, module, provide
                    (2vals expr null)])))))
         
         (define (annotate/top-level expr)
           (let*-2vals ([(annotated dont-care)
                         (top-level-annotate/inner (top-level-rewrite expr))])
             annotated)))
         
         ; body of local
      (let* ([annotated-expr (annotate/top-level expr)])
        ;(fprintf (current-error-port) "annotated: ~n~a~n" (syntax-object->datum annotated-expr))
        (values annotated-expr (list struct-proc-names user-defined-names binding-index))))))
