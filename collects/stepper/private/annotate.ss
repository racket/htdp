(module annotate mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "contract.ss")
	   (lib "list.ss")
           (lib "etc.ss")
           "marks.ss"
           "shared.ss"
           "my-macros.ss"
           (prefix beginner-defined: "beginner-defined.ss"))

  ; CONTRACTS
  
  (define annotate-opts-list?
    (and/c (listof (listof symbol?)) (lx (<= (length _) 1))))
  (define-struct annotate-environment (struct-proc-names pre-defined-names binding-index))
  
  ; PROVIDE
  (provide/contract
   [make-initial-env-package (-> annotate-environment?)]
   [annotate
    (->* (syntax?                               ; syntax to annotate
          annotate-environment?                 ; environment
          break-contract                        ; procedure for runtime break
          (symbols 'foot-wrap))                 ; wrap-kind
         annotate-opts-list?                    ; optional args
         (syntax? annotate-environment?))]      ; results
   [top-level-rewrite (-> syntax? syntax?)])
 
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
    (map (lx (map fn _)) lolst))
  
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

      
  (define (interlace a b)
    (foldr (lambda (a b built)
             (cons a (cons b built)))
           null
           a
           b))
        
  (define (closure-key-maker closure)
    closure)
  

  
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
  
  (define (make-initial-env-package) (make-annotate-environment null (namespace-mapped-symbols) 0))
  
  
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
    (let loop ([stx stx]
               [let-bound-bindings null]
               [macro-bound-bindings null]
               [cond-test (lx #f)]
               [and/or-test (lx #f)])
      (if (syntax-property stx 'stepper-skip-completely)
          stx
          (let* ([recur-regular 
                  (lambda (stx)
                    (loop stx let-bound-bindings macro-bound-bindings (lx #f) (lx #f)))]
                 [recur-with-bindings
                  (lambda (exp vars)
                    (loop exp (append vars let-bound-bindings) macro-bound-bindings (lx #f) (lx #f)))]
                 [recur-with-macro-bindings
                  (lambda (exp vars)
                    (loop exp let-bound-bindings (append vars macro-bound-bindings) (lx #f) (lx #f)))]
                 [recur-in-cond
                  (lambda (stx new-cond-test)
                    (loop stx let-bound-bindings macro-bound-bindings new-cond-test (lx #f)))]
                 [recur-in-and/or
                  (lambda (stx new-and/or-test new-bindings)
                    (loop stx let-bound-bindings (append new-bindings macro-bound-bindings) (lx #f) new-and/or-test))]
                 [do-let/rec
                  (lambda (stx rec?)
                    (with-syntax ([(label ((vars rhs) ...) . bodies) stx])
                      (let* ([vars-list (apply append (map syntax->list (syntax->list (syntax (vars ...)))))]
                             [labelled-vars-list (map (lambda (var-list) (map (lambda (exp) (recur-with-bindings exp vars-list))
                                                                              (syntax->list var-list)))
                                                      (syntax->list (syntax (vars ...))))]
                             [rhs-list (if rec?
                                           (map (lambda (exp) (recur-with-bindings exp vars-list)) (syntax->list #'(rhs ...)))
                                           (map recur-regular (syntax->list #'(rhs ...))))]
                             [new-bodies (map (lambda (exp) (recur-with-bindings exp vars-list)) (syntax->list #'bodies))]
                             [new-bindings (map list labelled-vars-list rhs-list)])
                        (datum->syntax-object stx `(,#'label ,new-bindings ,@new-bodies) stx stx))))])
            (kernel:kernel-syntax-case stx #f
              
              ; cond :
              [(if test (begin then) else-stx)
               (let ([origin (syntax-property stx 'origin)]
                     [rebuild-if
                      (lambda (new-cond-test)
                         (let* ([new-then (recur-regular (syntax then))]
                                [rebuilt (syntax-property
                                          (rebuild-stx `(if ,(recur-regular (syntax test))
                                                            ,new-then
                                                            ,(recur-in-cond (syntax else-stx) new-cond-test))
                                                       stx)
                                          'stepper-hint
                                         'comes-from-cond)])
                           ; move the stepper-else mark to the if, if it's present:
                           (if (syntax-property (syntax test) 'stepper-else)
                               (syntax-property rebuilt 'stepper-else #t)
                               rebuilt)))])
                 (cond [(cond-test stx) ; continuing an existing 'cond'
                        (rebuild-if cond-test)]
                       [(and origin (pair? origin) (eq? (syntax-e (car origin)) 'cond)) ; starting a new 'cond'
                        (rebuild-if (lambda (test-stx) 
                                      (and (eq? (syntax-source stx) (syntax-source test-stx))
                                           (eq? (syntax-position stx) (syntax-position test-stx)))))]
                       [else ; not from a 'cond' at all.
                        (rebuild-stx `(if ,@(map recur-regular (list (syntax test) (syntax (begin then)) (syntax else-stx)))) stx)]))]
              [(begin body) ; else clauses of conds; ALWAYS AN ERROR CALL
               (cond-test stx)
               (syntax-property stx 'stepper-skip-completely #t)]
              
              ; wrapper on a local.  This is necessary because teach.ss expands local into a trivial let wrapping a bunch of
              ;  internal defines, and therefore the letrec-values on which I want to hang the 'stepper-hint doesn't yet
              ;  exist.  So we patch it up after expansion.  And we discard the outer 'let' at the same time.
              [(let-values () expansion-of-local)
               (eq? (syntax-property stx 'stepper-hint) 'comes-from-local)
               (syntax-case #`expansion-of-local (letrec-values)
                 [(letrec-values (bogus-clause clause ...) . bodies)
                  (recur-regular
                   (syntax-property #`(letrec-values (clause ...) . bodies) 'stepper-hint 'comes-from-local))]
                 [else (error 'top-level-rewrite "expected a letrec-values inside a local, given: ~e" 
                              (syntax-object->datum #`expansion-of-local))])]
              
              ; let/letrec :
              [(let-values x ...) (do-let/rec stx #f)]
              [(letrec-values x ...) (do-let/rec stx #t)]
              [var
               (identifier? (syntax var))
               (syntax-property 
                (syntax var) 
                'stepper-binding-type
                (if (eq? (identifier-binding (syntax var)) 'lexical)
                    (cond [(ormap (lx (bound-identifier=? _ (syntax var))) let-bound-bindings)
                           'let-bound]
                          [(ormap (lx (bound-identifier=? _ (syntax var))) macro-bound-bindings)
                           'macro-bound]
                          [else
                           'lambda-bound])
                    'non-lexical))]
              [stx
               (let ([content (syntax-e (syntax stx))])
                 (if (pair? content)
                     (datum->syntax-object (syntax stx)
                                           (syntax-pair-map content recur-regular)
                                           (syntax stx)
                                           (syntax stx))
                     content))])))))
  
                                                 
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
           ((define foot-wrap? (eq? wrap-style 'foot-wrap))
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
                (break (current-continuation-marks) kind returned-value-list)))
            
            (define normal-break
              (make-break 'normal-break))
            
            (define result-exp-break
              (make-break 'result-exp-break))
            
            (define result-value-break
              (make-break 'result-value-break))
            
            ; struct-proc-names may be mutated
            (define struct-proc-names (annotate-environment-struct-proc-names annotate-environment))
            
            (define pre-defined-names (annotate-environment-pre-defined-names annotate-environment))
            
            (define binding-index (annotate-environment-binding-index annotate-environment))
            
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
              #`(with-continuation-mark #,debug-key #,debug-info #,expr))
            
            ; wcm-pre-break-wrap : call simple-wcm-wrap with a pre-break on the expr
            (define (wcm-pre-break-wrap debug-info expr)
              (simple-wcm-wrap debug-info #`(begin (#,result-exp-break) #,expr)))
            
            (define (break-wrap expr)
              #`(begin (#,normal-break) #,expr))
            
            (define (double-break-wrap expr)
              #`(begin (#,(make-break 'double-break)) #,expr))
            
            (define (late-let-break-wrap var-names lifted-gensyms expr)
              (let* ([interlaced (apply append (map list var-names lifted-gensyms))])
                #`(begin (#,(make-break 'late-let-break) #,@interlaced) #,expr)))
            
            
            (define (return-value-wrap expr)
              #`(let* ([result #,expr])
                  (#,result-value-break result)
                  result))
            
            ;  For Multiple Values:         
            ;           `(#%call-with-values
            ;             (#%lambda ()
            ;              expr)
            ;             (#%lambda result-values
            ;              (,(make-break 'result-break) result-values)
            ;              (#%apply #%values result-values))))
            
            
            (define (non-annotated-proc? varref)
              (kernel:kernel-syntax-case varref #f
                [(#%top . id)
                 (or
                  (memq (syntax-e (syntax id)) pre-defined-names)
                  (ormap (lx (module-identifier=? (syntax id) _)) struct-proc-names))]
                [id
                 (identifier? (syntax id))
                 (case (identifier-binding (syntax id))
                   [(lexical) #f] ; this only works in beginner
                   [(#f) (memq (syntax-e (syntax id)) pre-defined-names)]
                   [else #t])] ; module-bound vars
                [else
                 #f]))
            
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
            
            (define/contract annotate/inner
               (-> syntax? binding-set? boolean? boolean? (union false? syntax? (list/p syntax? syntax?)) (vector/p syntax? binding-set?))
               (lambda (expr tail-bound pre-break? top-level? procedure-name-info)
                 
                 (when (syntax-property expr 'stepper-define-struct-hint)
                   (let ([struct-names (car (syntax-property expr 'stepper-define-struct-hint))])
                     (set! struct-proc-names
                           (append struct-proc-names struct-names))))
                 
                 (cond [(syntax-property expr 'stepper-skipto)
                        (let* ([free-vars-captured #f] ; this will be set!'ed
                               ; WARNING! I depend on the order of evaluation in application arguments here:
                               [annotated (skipto-annotate
                                           (syntax-property expr 'stepper-skipto) 
                                           expr 
                                           (lambda (subterm)
                                             (let*-2vals ([(stx free-vars) (annotate/inner subterm tail-bound pre-break? top-level? procedure-name-info)])
                                                         (set! free-vars-captured free-vars)
                                                         stx)))])
                          (2vals (if top-level?
                                     annotated
                                     (simple-wcm-wrap
                                      skipto-mark
                                      annotated))
                                 free-vars-captured))]
                       
                       [(syntax-property expr 'stepper-skip-completely)
                        (if top-level?
                              (2vals expr null)
                              (2vals (simple-wcm-wrap 13 expr) null))]
                     
                       [else
                        (let* ([tail-recur (lambda (expr) (annotate/inner expr tail-bound #t #f procedure-name-info))]
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
                               [inferred-name-patch (lambda (annotated)
                                                      (printf "procedure-name-info: ~a\n bool-test: ~a\n" procedure-name-info (if procedure-name-info #t #f))
                                                      (if (and procedure-name-info (not (memq 'no-closure-capturing wrap-opts)))
                                                          (let ([name (ccond [(syntax? procedure-name-info) procedure-name-info]
                                                                             [(and (list? procedure-name-info)
                                                                                   (= (length procedure-name-info) 2))
                                                                              (car procedure-name-info)])])
                                                            #`(let* ([#,name #,annotated]) #,name))
                                                          annotated))]
                               
                               [normal-bundle
                                (lambda (free-vars annotated)
                                  (2vals (wcm-wrap (make-debug-info-normal free-vars)
                                                   annotated)
                                         free-vars))]
                               
                               [lambda-clause-abstraction 
                                (lambda (clause)
                                  (with-syntax ([(args-stx . bodies) clause])
                                    (let*-2vals ([(annotated-body free-varrefs)
                                                  ; wrap bodies in explicit begin if more than 1 user-introduced (non-skipped) bodies
                                                  (if (> (length (filter (lambda (clause)
                                                                           (not (syntax-property clause 'stepper-skip-completely)))
                                                                         (syntax->list (syntax bodies)))) 1)
                                                      (lambda-body-recur (syntax (begin . bodies)))
                                                      (let*-2vals ([(annotated-bodies free-var-sets)
                                                                    (2vals-map lambda-body-recur (syntax->list #`bodies))])
                                                        (2vals #`(begin . #,annotated-bodies) (varref-set-union free-var-sets))))]
                                                 [new-free-varrefs (varref-set-remove-bindings free-varrefs
                                                                                               (arglist-flatten #'args-stx))])
                                                (2vals (datum->syntax-object #'here `(,#'args-stx ,annotated-body) #'clause) new-free-varrefs))))]
                               
                               [outer-lambda-abstraction
                                (lambda (annotated-lambda free-varrefs)
                                  (let*-2vals
                                      ([closure-info (make-debug-info-app 'all free-varrefs 'none)]
                                       [closure-name (cond [(syntax? procedure-name-info) procedure-name-info]
                                                           [(pair? procedure-name-info) (car procedure-name-info)]
                                                           [else #f])]
                                       [closure-storing-proc
                                        (opt-lambda (closure debug-info [lifted-index #f])
                                          (closure-table-put! closure (make-closure-record 
                                                                       closure-name
                                                                       debug-info
                                                                       #f
                                                                       lifted-index))
                                           closure)]
                                       [inferred-name-lambda
                                        (if closure-name
                                            (syntax-property annotated-lambda 'inferred-name closure-name)
                                            annotated-lambda)]
                                       [captured
                                        (if (memq 'no-closure-capturing wrap-opts)
                                            inferred-name-lambda
                                            (cond [(pair? procedure-name-info)
                                                   #`(#,closure-storing-proc #,inferred-name-lambda #,closure-info 
                                                       #,(cadr procedure-name-info))]
                                                  [else
                                                   #`(#,closure-storing-proc #,inferred-name-lambda #,closure-info)]))])
                                    
                                    (normal-bundle free-varrefs captured)))]
                               
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
                                  (with-syntax ([(_ ([(var ...) val] ...) . bodies) stx])
                                    (let*-2vals
                                     ([binding-sets (map syntax->list (syntax->list #'((var ...) ...)))]
                                      [binding-list (apply append binding-sets)]
                                      [vals (syntax->list #'(val ...))]
                                      [lifted-var-sets (map (lx (map get-lifted-var _)) binding-sets)]
                                      [lifted-vars (apply append lifted-var-sets)]
                                      [(annotated-vals free-varref-sets-vals)
                                       (2vals-map let-rhs-recur vals binding-sets lifted-var-sets binding-sets)]
                                      [(annotated-body free-varrefs-body)
                                       ((let-body-recur binding-list) 
                                        (if (= (length (syntax->list (syntax bodies))) 1)
                                            (car (syntax->list (syntax bodies)))
                                            (syntax (begin . bodies))))]
                                      [free-varrefs (varref-set-remove-bindings 
                                                     (varref-set-union (cons free-varrefs-body
                                                                             free-varref-sets-vals)) 
                                                     binding-list)])
                                     
                                      (let* ([unevaluated-list (make-init-list binding-list)]
                                             [outer-initialization
                                              #`([(#,@lifted-vars #,@binding-list #,let-counter)
                                                   (values #,@(append (map (lambda (binding)
                                                                             #`(#,binding-indexer)) 
                                                                           binding-list)
                                                                      unevaluated-list
                                                                      (list 0)))])]
                                             [counter-clauses (build-list 
                                                               (length binding-sets)
                                                               (lambda (num)
                                                                 #`(set! #,let-counter #,(+ num 1))))]
                                             [set!-clauses
                                              (map (lambda (binding-set val)
                                                     #`(set!-values #,binding-set #,val))
                                                   binding-sets
                                                   annotated-vals)] 
                                             ; time to work from the inside out again
                                             ; without renaming, this would all be much much simpler.
                                             [wrapped-begin (wcm-wrap (make-debug-info-let free-varrefs
                                                                                           binding-list
                                                                                           let-counter) 
                                                                      (double-break-wrap
                                                                       #`(begin #,@(apply append (zip set!-clauses counter-clauses)) 
                                                                                #,(late-let-break-wrap binding-list
                                                                                                       lifted-vars
                                                                                                       annotated-body))))])
                                        (2vals (quasisyntax/loc 
                                                expr 
                                                (#,output-identifier #,outer-initialization #,wrapped-begin)) 
                                               free-varrefs)))))]
                               
                               ; if-abstraction: (-> syntax? syntax? (union false? syntax?) (values syntax? varref-set?))
                               [if-abstraction
                                (lambda (test then else) 
                                  (let*-2vals
                                   ([(annotated-test free-varrefs-test) 
                                     (non-tail-recur test)]
                                    [(annotated-then free-varrefs-then) 
                                     (tail-recur then)]
                                    [(annotated-else free-varrefs-else)
                                     (if else
                                         (tail-recur else)
                                         (2vals #f null))]
                                    [free-varrefs (varref-set-union (list free-varrefs-test 
                                                                          free-varrefs-then 
                                                                          free-varrefs-else))]
                                    [annotated-if 
                                     #`(begin (set! #,if-temp #,annotated-test) 
                                              (#,normal-break)
                                              #,(if else
                                                    (quasisyntax/loc expr (if #,if-temp #,annotated-then #,annotated-else))
                                                    (quasisyntax/loc expr (if #,if-temp #,annotated-then))))]
                                    [wrapped (wcm-wrap (make-debug-info-app (binding-set-union (list tail-bound (list if-temp)))
                                                                            (varref-set-union (list free-varrefs (list if-temp)))
                                                                            'none)
                                                       annotated-if)])
                                   (2vals
                                    (with-syntax ([test-var if-temp]
                                                  [wrapped-stx wrapped]
                                                  [unevaluated-stx *unevaluated*])
                                      (syntax/loc expr (let ([test-var unevaluated-stx]) wrapped-stx)))
                                    free-varrefs)))]
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
                            
                            
                              
                            [(if test then else) (if-abstraction (syntax test) (syntax then) (syntax else))]
                            [(if test then) (if-abstraction (syntax test) (syntax then) #f)]
                            
                            [(begin . bodies-stx)
                             (if top-level? 
                                    (let*-2vals
                                        ([(annotated-bodies free-varref-sets)
                                          (2vals-map (lambda (expr)
                                                       (top-level-annotate/inner expr)) 
                                                     (syntax->list (syntax bodies-stx)))])
                                      (2vals (quasisyntax/loc expr (begin #,@annotated-bodies))
                                             (varref-set-union free-varref-sets)))
                                    (if (null? (syntax->list (syntax bodies-stx)))
                                        (normal-bundle null expr)
                                        (let*-2vals 
                                            ([reversed-bodies (reverse (syntax->list (syntax bodies-stx)))]
                                             [last-body (car reversed-bodies)]
                                             [all-but-last (reverse (cdr reversed-bodies))]
                                             [(annotated-a free-varrefs-a)
                                              (2vals-map non-tail-recur all-but-last)]
                                             [(annotated-final free-varrefs-final)
                                              (tail-recur last-body)])
                                          (normal-bundle (varref-set-union (cons free-varrefs-final free-varrefs-a))
                                                         (quasisyntax/loc expr (begin #,@annotated-a #,annotated-final))))))]
                            
                            [(begin0 . bodies-stx)
                             (let*-2vals
                                 ([bodies (syntax->list (syntax bodies-stx))]
                                  [(annotated-first free-varrefs-first)
                                   (result-recur (car bodies))]
                                  [(annotated-bodies free-varref-sets)
                                   (2vals-map non-tail-recur (cdr bodies))])
                               (normal-bundle (varref-set-union (cons free-varrefs-first free-varref-sets))
                                              (quasisyntax/loc expr (begin0 #,annotated-first #,@annotated-bodies))))]
                            
                            [(let-values . _)
                             (let*-2vals ([collapsed (collapse-let-values expr)])
                                         (let-abstraction collapsed 
                                                          'let*-values
                                                          (lambda (bindings)
                                                            (map (lambda (_) *unevaluated*) bindings))))]
                            
                            [(letrec-values . _)
                             (let-abstraction expr 
                                              'letrec-values
                                              (lambda (bindings) (map (lambda (b) #`#,b) bindings)))]
                            
                            [(set! var val)
                             (let*-2vals
                                 ([(annotated-val val-free-varrefs)
                                   (set!-rhs-recur (syntax val) (syntax-case (syntax var) (#%top)
                                                                  [(#%top . real-var) (syntax-e (syntax real-var))]
                                                                  [else (syntax var)]))])
                               (normal-bundle (varref-set-union (list (list (syntax var)) val-free-varrefs))
                                              (quasisyntax/loc expr (set! #,(syntax var) #,annotated-val))))]
                            
                            
                            [(quote _)
                             (normal-bundle null expr)]
                            
                            [(quote-syntax _)
                             (normal-bundle null expr)]
                            
                            [(with-continuation-mark key mark body)
                             (let*-2vals ([(annotated-key free-varrefs-key)
                                           (non-tail-recur (syntax key))]
                                          [(annotated-mark free-varrefs-mark)
                                           (non-tail-recur (syntax mark))]
                                          [(annotated-body free-varrefs-body)
                                           (result-recur (syntax body))])
                               (error 'annotate/inner "this region of code is still under construction")
                               
                               ;                                       [annotated #`(let-values ([key-temp #,*unevaluated*]
                               ;                                             [mark-temp #,*unevaluated*]
                               )]
                            
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
                               [annotated (quasisyntax/loc expr (#,@annotated-terms))])
                              (2vals
                               ;(if (and ankle-wrap?
                               ;         (memq 'no-temps-for-varrefs wrap-opts)
                               ;         (andmap term-is-reduced
                               ;                 (syntax->list (syntax terms))))
                                   
                                   ; this is the no-temps optimization:
                                   ; (won't work for stepper unless no reductions happen on the vars in the app
                                   ; oh! what if they're all lambda-bound vars? some other day, perhaps.
                                   
                                   ;(let ([debug-info (make-debug-info-app tail-bound free-varrefs 'called)])
                                   ;  (wcm-break-wrap debug-info annotated-terms))
                                   
                               (let* ([arg-temps (build-list (length annotated-terms) get-arg-var)]
                                      [tagged-arg-temps (map (lambda (var) (syntax-property var 'stepper-binding-type 'stepper-temp))
                                                             arg-temps)]
                                      [let-clauses #`((#,tagged-arg-temps 
                                                       (values #,@(map (lambda (_) *unevaluated*) tagged-arg-temps))))]
                                      [set!-list (map (lambda (arg-symbol annotated-sub-expr)
                                                        #`(set! #,arg-symbol #,annotated-sub-expr))
                                                      tagged-arg-temps annotated-terms)]
                                      [new-tail-bound (binding-set-union (list tail-bound tagged-arg-temps))]
                                      [app-debug-info (make-debug-info-app new-tail-bound tagged-arg-temps 'called)]
                                      [app-term (quasisyntax/loc expr #,tagged-arg-temps)]
                                      [debug-info (make-debug-info-app new-tail-bound
                                                                       (varref-set-union (list free-varrefs tagged-arg-temps)) ; NB using bindings as vars
                                                                       'not-yet-called)]
                                      [let-body (wcm-wrap debug-info #`(begin #,@set!-list
                                                                              #,(break-wrap
                                                                                 (simple-wcm-wrap
                                                                                  app-debug-info
                                                                                  #`(if (#,in-closure-table #,(car tagged-arg-temps))
                                                                                        #,app-term
                                                                                        #,(return-value-wrap app-term))))))])
                                 #`(let-values #,let-clauses #,let-body))
                                   ;)
                               free-varrefs))]   
                            
                            [(#%datum . _)
                             (normal-bundle null expr)]
                            
                            [(define-values vars-stx body)
                             (let*-2vals
                              ([vars (syntax->list (syntax vars-stx))]
                               [(annotated-val free-varrefs-val)
                                (begin
                                  (printf "vars: ~a\n" (map syntax-e vars))
                                  (define-values-recur (syntax body) (if (not (null? vars))
                                                                         (car vars)
                                                                       #f)))])
                              (2vals
                               (quasisyntax/loc expr (define-values vars-stx #,annotated-val))
                               free-varrefs-val))]
                            
                            
                            [(#%top . var-stx)
                             (let*-2vals ([var (syntax var-stx)]
                                          [free-varrefs null])
                                         (2vals 
                                          (if (and (memq (syntax-e var) pre-defined-names)
                                                   (not (memq (syntax-e var) beginner-defined:must-reduce)))
                                              (wcm-wrap (make-debug-info-normal free-varrefs) expr)
                                              (wcm-break-wrap (make-debug-info-normal free-varrefs)
                                                              (return-value-wrap expr)))
                                          free-varrefs))]
                            
                            [var-stx
                             (identifier? (syntax var-stx))
                             (let*-2vals ([var (syntax var-stx)]
                                          [free-varrefs (list var)])
                                         (2vals 
                                          (case (syntax-property var 'stepper-binding-type)
                                            ((lambda-bound macro-bound) 
                                             (wcm-wrap (make-debug-info-normal free-varrefs) var))
                                            ((let-bound) 
                                             (wcm-break-wrap (make-debug-info-normal free-varrefs)
                                                             (return-value-wrap var)))
                                            ((non-lexical) 
                                             (case (identifier-binding var)
                                               ((#f) (error 'annotate "top-level identifier occurs without #%top"))
                                               (else (if (memq (syntax-e var) beginner-defined:must-reduce)
                                                         (wcm-break-wrap (make-debug-info-normal free-varrefs)
                                                                         (return-value-wrap var))
                                                         (wcm-wrap (make-debug-info-normal free-varrefs) var))))))
                                          free-varrefs))]
                            
                            [else ; require, require-for-syntax, define-syntaxes, module, provide
                             (2vals expr null)]))])))
            
            (define (annotate/top-level expr)
              (let*-2vals ([(annotated dont-care)
                            (top-level-annotate/inner (top-level-rewrite expr))])
                annotated)))
         
         ; body of local
         (let* ([annotated-expr (annotate/top-level expr)])
           (fprintf (current-error-port) "annotated: ~n~a~n" (syntax-object->datum annotated-expr))
           (values annotated-expr (make-annotate-environment struct-proc-names pre-defined-names binding-index))))))
