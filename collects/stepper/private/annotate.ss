(module annotate mzscheme
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (prefix utils: "utils.ss")
           "marks.ss"
           ;(prefix s: "model.ss")
           "shared.ss"
	   (lib "list.ss")
           (lib "etc.ss")
           "my-macros.ss")

; for testing:
  (define (s:check-pre-defined-var x)
    (eq? (car (symbol->string (syntax-e x))) #\a))
  
  (provide
   initial-env-package
   annotate)

;  (import [z : zodiac^]
;          [utils : stepper:cogen-utils^]
;          stepper:marks^
;          [s : stepper:model^]
;	  stepper:shared^
;          stepper:client-procs^)

  
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;           Support Functions                ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
 
  ;; this looks wrong...
  (define (internal-error . x)
    (error 'annotater-internal-error "~s" x))

  ;; d->so just replicates the exp argument in a call to datum->syntax-object
  (define (d->so exp datum)
    (datum->syntax-object exp datum exp))
  
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

  ; a BINDING is a syntax-object
  ; a VARREF is a syntax-object
  
  ; a BINDING-SET is (union 'all (listof BINDING))
  ; a VARREF-SET is (listof VARREF)
  
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
              (set-pair-union a-set b-set eq?))
            
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
  ; return the subset of bindings that appear in the varrefs
  
  (define (binding-set-varref-set-intersect bindings varrefs)
    (cond [(eq? bindings 'all) varrefs]
          [else (filter (lambda (binding)
                          (ormap (lambda (varref)
                                   (bound-identifier=? binding varref))
                                 varrefs))
                        bindings)]))
  
  ; varref-set-remove-bindings : VARREF-SET (BINDING-SET - 'all) -> VARREF-SET
  ; remove bindings from varrefs
  
  (define (varref-set-remove-bindings varrefs bindings)
    (cond [(eq? bindings 'all)
           (error 'varref-set-remove-bindings "binding-set 'all passed as second argument, first argument was: ~s" varrefs)]
          [else (remove* bindings varrefs bound-identifier=?)]))
      
  (define (get-arg-var n)
    (string->symbol (format "arg-temp-~a" n)))
  
  ; WARNING: because of how syntax-property works, these properties will have a default value of #f.
  ; that's what we want, in this case.
  
  (define (never-undefined? stx)
    (syntax-property stx 'never-undefined))
  (define (mark-never-undefined stx) 
    (syntax-property stx 'never-undefined #t))

  (define (lambda-bound-var? stx)
    (syntax-property stx 'lambda-bound-var))
  (define (mark-lambda-bound-var stx)
    (syntax-property stx 'lambda-bound-var #t))
  
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
  ;; (syntax-object BINDING-SET BINDING-SET BINDING-SET symbol boolean) -> debug-info)
  ;;
  ;;;;;;;;;;
     
  (define (make-debug-info source tail-bound free-bindings advance-warning label lifting?)
    (let* ([kept-bindings (if (eq? tail-bound 'all)
                              free-bindings
                              (binding-set-varref-set-intersect tail-bound
                                                     free-bindings))]
           [var-clauses (map (lambda (x) 
                               (list x (d->so #f `(quote-syntax ,x))))
                             kept-bindings)]
           [let-bindings (filter (lambda (x) (not (lambda-bound-var? x))) 
                                 (append advance-warning kept-bindings))]
           [lifter-syms (map get-lifted-var let-bindings)]
           [quoted-lifter-syms (map (lambda (b) 
                                      (d->so #f `(syntax-quote ,b))) 
                                    lifter-syms)]
           [let-clauses (map list lifter-syms quoted-lifter-syms)])
      (make-full-mark source label (append var-clauses (if lifting? let-clauses null)))))
  
  ; cheap-wrap for non-debugging annotation
  
  (define (cheap-wrap body)
    (d->so #f `(with-continuation-mark ,debug-key
                                       ,(make-cheap-mark (d->so #f `(quote-syntax ,body)))
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
  
  (define initial-env-package null)
  
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
      [(syntax-quote _) #t]
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
;  
;        * * * * * *      AAAA  N    N N    N  OOOO  TTTTT  AAAA  TTTTT EEEEEE     * * * * * * 
;         ***   ***      A    A NN   N NN   N O    O   T   A    A   T   E           ***   ***  
;        ***** *****     AAAAAA N N  N N N  N O    O   T   AAAAAA   T   EEEE       ***** ***** 
;         ***   ***      A    A N  N N N  N N O    O   T   A    A   T   E           ***   ***  
;        * * * * * *     A    A N   NN N   NN  OOOO    T   A    A   T   EEEEEE     * * * * * * 
;  
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
  
  (define (annotate read-expr expr input-struct-proc-names break wrap-style . wrap-opts-list)
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

         
         (define (make-break kind)
           (lambda returned-value-list
             (break (current-continuation-marks) debug-key kind returned-value-list)))

         ; wrap creates the w-c-m expression.
         
         (define (simple-wcm-wrap debug-info expr)
           (d->so #f `(with-continuation-mark ,debug-key ,debug-info ,expr)))
         
         (define (wcm-pre-break-wrap debug-info expr)
           (if break
               (simple-wcm-wrap debug-info (d->so #f `(begin (,(make-break 'result-break)) ,expr)))
               (simple-wcm-wrap debug-info expr)))
         
         (define (break-wrap expr)
           (if break
               (d->so #f `(begin (,(make-break 'normal-break)) ,expr))
               expr))
         
         (define (double-break-wrap expr)
           (if break
               (d->so #f `(begin (,(make-break 'double-break)) ,expr))
               expr))
         
         (define (simple-wcm-break-wrap debug-info expr)
           (simple-wcm-wrap debug-info (break-wrap expr)))
         
         (define (late-let-break-wrap var-names lifted-gensyms expr)
           (if break
               (let* ([interlaced (apply append (map list var-names lifted-gensyms))])
                 (d->so expr `(begin (,(make-break 'late-let-break) ,@interlaced) ,expr)))
               expr))
         
         (define (return-value-wrap expr)
           (if break
               (d->so expr
                      `(let* ([result ,expr])
                         (,(make-break 'result-break) result)
                         result))
               expr))

;  For Multiple Values:         
;           `(#%call-with-values
;             (#%lambda ()
;              expr)
;             (#%lambda result-values
;              (,(make-break 'result-break) result-values)
;              (#%apply #%values result-values))))

         
;         (define (find-read-expr expr)
;           (when (not (z:zodiac? expr))
;             (error 'find-read-expr "argument to find-read-expr is not a zodiac expr: ~a" expr))
;           (let ([offset (z:location-offset (z:zodiac-start expr))])
;             (let search-exprs ([exprs red-exprs])
;               (let* ([later-exprs (filter 
;                                    (lambda (expr) 
;                                      (<= offset (z:location-offset (z:zodiac-finish expr))))
;                                    exprs)]
;                      [expr 
;                       (car later-exprs)])
;                 (if (= offset (z:location-offset (z:zodiac-start expr)))
;                     expr
;                     (cond
;                       ((z:scalar? expr) (internal-error expr "starting offset inside scalar:" offset))
;                       ((z:sequence? expr) 
;                        (let ([object (z:read-object expr)])
;                            (cond
;                            ((z:list? expr) (search-exprs object))
;                            ((z:vector? expr) 
;                             (search-exprs (vector->list object))) ; can source exprs be here?
;                            ((z:improper-list? expr)
;                             (search-exprs (search-exprs object))) ; can source exprs be here? (is this a bug?)
;                            (else (internal-error expr "unknown expression type in sequence")))))
;                       (else (internal-error expr "unknown read type"))))))))
;  
         (define (struct-procs-defined expr)
           (if (andmap (lambda (origin-entry)
                         (eq? (syntax-e origin-entry) 'define-struct))
                       (syntax-property expr 'origin))
               (syntax-case expr (define-values)
                 [(define-values vars body)
                  (syntax->list (syntax vars))]
                 [else
                  null])
               null))
         
         (define struct-proc-names (append (struct-procs-defined expr)
                                           input-struct-proc-names))
         
         (define (non-annotated-proc? varref)
           (let ([name (syntax-e varref)])
             (or (and (s:check-pre-defined-var name)
                      (not (eq? name 'apply)))
                 (memq name struct-proc-names))))
         
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
         
	 (define (annotate/inner expr tail-bound pre-break? top-level? procedure-name-info)
	   
	   (let* ([tail-recur (lambda (expr) (annotate/inner expr tail-bound #t #f procedure-name-info))]
                  [define-values-recur (lambda (expr name) 
                                         (annotate/inner expr tail-bound #f #f name))]
                  [non-tail-recur (lambda (expr) (annotate/inner expr null #f #f #f))]
                  [result-recur (lambda (expr) (annotate/inner expr null #f #f procedure-name-info))]
                  [set!-rhs-recur (lambda (expr name) (annotate/inner expr null #f #f name))]
                  [let-rhs-recur (lambda (expr bindings dyn-index-syms)
                                   (let* ([proc-name-info 
                                           (if (not (null? bindings))
                                               (list (car bindings) (car dyn-index-syms))
                                               #f)])
                                     (annotate/inner expr tail-bound #f #f proc-name-info)))]
                  [lambda-body-recur (lambda (expr) (annotate/inner expr 'all #t #f #f))]
                  ; note: no pre-break for the body of a let; it's handled by the break for the
                  ; let itself.
                  [let-body-recur (lambda (bindings)
                                    (lambda (expr) 
                                      (annotate/inner expr (binding-set-union (list tail-bound bindings)) #f #f procedure-name-info)))]
                  [cheap-wrap-recur (lambda (expr) (let-values ([(ann _) (tail-recur expr)]) ann))]
                  [no-enclosing-recur (lambda (expr) (annotate/inner expr 'all #f #f #f))]
                  [make-debug-info-normal (lambda (free-bindings)
                                            (make-debug-info expr tail-bound free-bindings null 'none foot-wrap?))]
                  [make-debug-info-app (lambda (tail-bound free-bindings label)
                                         (make-debug-info expr tail-bound free-bindings null label foot-wrap?))]
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
                                               (d->so #f `(let* ([,name ,annotated]) ,name)))
                                             annotated))]
                  
                  
;                  
;                  l        AAAA  M    M BBBBB  DDDDD   AAAA
;                  l       A    A MM  MM B    B D    D A    A
;                  l       AAAAAA M MM M BBBBB  D    D AAAAAA
;                  l       A    A M    M B    B D    D A    A
;                  llllll  A    A M    M BBBBB  DDDDD  A    A
                  
                  [lambda-clause-abstraction 
                   (lambda (clause)
                     (with-syntax ([(args body ...) clause])
                       (utils:improper-foreach mark-never-undefined (syntax args))
                       (let*-2vals ([(annotated-bodies free-varref-sets)
                                     (2vals-map lambda-body-recur (syntax (body ...)))]
                                    [new-free-varrefs (varref-set-remove-bindings (varref-set-union free-varref-sets)
                                                                                  (syntax args))])
                         (with-syntax ([annotated-bodies annotated-bodies])
                           (2vals (syntax/loc clause (args . annotated-bodies)) free-varref-sets)))))]
                  
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
                             [captured
                              (if (memq 'no-closure-capturing wrap-opts)
                                  annotated-lambda
                                  (cond [(symbol? procedure-name-info)
                                         (d->so expr `(,closure-storing-proc ,annotated-lambda ,closure-info))]
                                        [(pair? procedure-name-info)
                                         (if foot-wrap?
                                             (d->so expr `(,closure-storing-proc ,annotated-lambda ,closure-info 
                                                           ,(cadr procedure-name-info)))
                                             (d->so expr `(,closure-storing-proc ,annotated-lambda ,closure-info 
                                                           #f)))]
                                        [else
                                         (d->so expr `(,closure-storing-proc ,annotated-lambda ,closure-info))]))])
                          (cond [(symbol? procedure-name-info)
                                 (syntax-property annotated-lambda 'inferred-name procedure-name-info)]
                                [(pair? procedure-name-info)
                                 (syntax-property annotated-lambda 'inferred-name (car procedure-name-info))])
                          (2vals
                           (ccond [foot-wrap? 
                                   (wcm-wrap (make-debug-info-normal free-varrefs)
                                             captured)]
                                  [ankle-wrap? 
                                   captured]) ; no wcm is needed because evaluation of closures cannot cause exceptions.
                           free-varrefs))))]
;                  
;                  L      EEEEEE TTTTTTT
;                  L      E         T
;                  L      EEEEEE    T
;                  L      E         T
;                  LLLLLL EEEEEE    T
                    
                  ; The let transformation is complicated.
                  ; here's a sample transformation (not including 'break's):
                  ;(let-values ([(a b c) e1] [(d e) e2]) e3)
                  ;
                  ;turns into
                  ;
                  ;(let-values ([(a b c d e)
                  ;              (values *unevaluated* *unevaluated* *unevaluated* *unevaluated* *unevaluated*)])
                  ;  (with-continuation-mark 
                  ;   key huge-value
                  ;   (begin
                  ;     (set!-values (a b c) e1)
                  ;     (set!-values (d e) e2)
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
                   (lambda (stx output-identifier check-fn make-init-list)
                     (with-syntax ([(_ ([var val] ...) . bodies) stx]
                                   [(_a (binding ...) . _b) stx])
                       (let*-2vals
                           ([binding-sets (map syntax->list (syntax->list (syntax (var ...))))]
                            [binding-list (foldl append null binding-sets)]
                            [vals (syntax->list (syntax (val ...)))]
                            [_1 (check-fn vals binding-list)]
                            [lifted-var-sets (map (lambda (x) (map get-lifted-var x)) binding-sets)]
                            [lifted-vars (apply append lifted-var-sets)]
                            [(annotated-vals free-varref-sets-vals)
                             (2vals-map let-rhs-recur vals binding-sets lifted-var-sets)]
                            [(annotated-bodies free-varref-sets-bodies)
                             (2vals-map (let-body-recur binding-list) (syntax->list (syntax bodies)))]
                            [free-bindings (varref-set-remove-bindings 
                                            (varref-set-union (append free-varref-sets-bodies 
                                                                      free-varref-sets-vals)) 
                                            binding-list)])
                       (ccond [cheap-wrap?
                               (let* ([bindings
                                       (map (lambda (binding-loc bindings val)
                                              (d->so binding-loc `(,bindings ,val)))
                                            (syntax->list (syntax (binding ...)))
                                            binding-sets
                                            annotated-vals)]
                                      [annotated
                                       (d->so expr `(,output-identifier ,bindings ,@annotated-bodies))])
                                 (2vals (appropriate-wrap annotated free-bindings) free-bindings))]
                              [(or ankle-wrap? foot-wrap?)
                               (let* ([unevaluated-list (make-init-list binding-list)]
                                      [outer-initialization
                                       (if ankle-wrap?
                                           (d->so #f `((,binding-list (values ,@unevaluated-list))))
                                           (d->so #f `([,(append lifted-vars binding-list)
                                                        (values ,@(append (map (lambda (binding)
                                                                                   (d->so #f `(,binding-indexer))) 
                                                                                 binding-list)
                                                                            unevaluated-list))])))]
                                      [set!-clauses
                                       (map (lambda (binding-set val)
                                              (d->so #f `(set!-values ,binding-set ,val)))
                                            binding-sets
                                            annotated-vals)]
                                      ; time to work from the inside out again
                                      ; without renaming, this would all be much much simpler.
                                      [middle-begin
                                       (double-break-wrap (d->so #f `(begin ,@set!-clauses 
                                                                            ,(late-let-break-wrap binding-list
                                                                                                  lifted-vars
                                                                                                  (car annotated-bodies))
                                                                            ,@(cdr annotated-bodies))))]
                                      [wrapped-begin
                                       (wcm-wrap (make-debug-info expr 
                                                                  (binding-set-union (list tail-bound binding-list))
                                                                  (varref-set-union (list free-bindings binding-list)) ; NB using bindings as varrefs
                                                                  null ; advance warning
                                                                  'let-body
                                                                  foot-wrap?)
                                                 middle-begin)]
                                      [whole-thing
                                       (d->so expr `(,output-identifier ,outer-initialization ,wrapped-begin))])
                                 (values whole-thing free-bindings))]))))]

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
                      (d->so expr `(if ,annotated-test ,annotated-then ,annotated-else))])
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
                      (d->so expr `(if ,annotated-test ,annotated-then))])
                  (2vals
                   (if foot-wrap?
                       (wcm-wrap (make-debug-info-normal free-varrefs) annotated-if)
                       (appropriate-wrap annotated-if free-varrefs))
                   free-varrefs))]
               
               [(begin . bodies-stx)
                (if top-level? 
                    (let*-2vals
                        ([(annotated-bodies free-varref-sets)
                          (2vals-map (lambda (expr)
                                       (top-level-annotate/inner expr)) 
                                     (syntax->list (syntax bodies-stx)))])
                      (2vals (d->so expr `(begin ,@annotated-bodies))
                             (varref-set-union free-varref-sets)))
                    (let*-2vals 
                        ([bodies (syntax->list (syntax bodies))]
                         [(all-but-last-body last-body-list) 
                          (list-partition bodies (- (length bodies) 1))]
                         [last-body (car last-body-list)]
                         [(annotated-a free-varrefs-a)
                          (2vals-map non-tail-recur all-but-last-body)]
                         [(annotated-final free-varrefs-final)
                          (tail-recur last-body)]
                         [free-varrefs (varref-set-union (cons free-varrefs-final free-varrefs-a))]
                         [debug-info (make-debug-info-normal free-varrefs)]
                         [annotated (d->so expr `(begin ,@(append annotated-a (list annotated-final))))])
                       (values (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-varrefs)]
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
                    [annotated (d->so expr `(begin0 ,annotated-first ,@annotated-bodies))])
                 (values (ccond [(or cheap-wrap? ankle-wrap?) (appropriate-wrap annotated free-varrefs)]
                                [foot-wrap?
                                 (wcm-wrap debug-info annotated)])
                         free-varrefs))]
               
               [(let-values . _)
                (let*-2vals ([collapsed (collapse-let-values expr)])
                  (let-abstraction collapsed 
                                   'let*-values
                                   (lambda (vals binding-list)
                                     (for-each mark-never-undefined binding-list))
                                   (lambda (bindings)
                                     (d->so #f (map (lambda (_) *unevaluated*) bindings)))))]
               
               [(letrec-values . _)
                (let-abstraction expr 
                                 'letrec-values 
                                 (lambda (vals binding-list)
                                   (when (andmap (lambda (stx)
                                                   (syntax-case stx (lambda)
                                                     [(lambda . stuff) #t]
                                                     [else #f]))
                                                   vals)
                                     (for-each mark-never-undefined binding-list)))
                                 (lambda (bindings) (d->so #f bindings)))]
               
               [(set! var val)
                (let*-2vals
                    ([(annotated-val val-free-varrefs)
                      (set!-rhs-recur (syntax val) (syntax-case (syntax var) (#%top)
                                                     [(#%top . real-var) (syntax-e (syntax real-var))]
                                                     [else (syntax var)]))]
                     [free-varrefs (varref-set-union (list (syntax-case (syntax var) (#%top)
                                                             [(#%top . real-var) null]
                                                             [else (list (syntax var))])
                                                           val-free-varrefs))]
                     [debug-info (make-debug-info-normal free-varrefs)]
                     [annotated (d->so expr `(set! ,(syntax var) ,annotated-val))])
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
                               [annotated (d->so expr `(with-continuation-mark ,annotated-key
                                                                               ,annotated-mark
                                                                               ,annotated-body))])
                            (2vals (appropriate-wrap annotated free-varrefs) free-varrefs))]
                         [foot-wrap?
                          (error 'annotate/inner "this region of code is still under construction")

;                                       [annotated (d->so #f `(let-values ([key-temp ,*unevaluated*]
;                                                                          [mark-temp ,*unevaluated*])
                                                               
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
                     [annotated (d->so expr annotated-terms)])
                  (2vals
                   (ccond [cheap-wrap? (appropriate-wrap annotated free-varrefs)]
                          [(or ankle-wrap? foot-wrap?)
                           (if (and ankle-wrap?
                                    (memq 'no-temps-for-varrefs wrap-opts)
                                    (andmap term-is-reduced
                                            (syntax->list (syntax terms))))
                               
                               ; this is the no-temps optimization:
                               ; (won't work for stepper unless no reductions happen on the vars in the app
                               
                               (let ([debug-info (make-debug-info-app tail-bound free-varrefs 'called)])
                                 (wcm-break-wrap debug-info annotated-terms))
                               
                               (let* ([arg-temps (build-list (length annotated-terms) get-arg-var)] 
                                      [let-clauses (d->so #f `((,arg-temps 
                                                                (values ,@(map (lambda (_) *unevaluated*) arg-temps)))))]
                                      [set!-list (map (lambda (arg-symbol annotated-sub-expr)
                                                        (d->so #f `(set! ,arg-symbol ,annotated-sub-expr)))
                                                      arg-temps annotated-terms)]
                                      [new-tail-bound (binding-set-union (list tail-bound arg-temps))]
                                      [app-debug-info (make-debug-info-app new-tail-bound arg-temps 'called)]
                                      [final-app (break-wrap (simple-wcm-wrap app-debug-info
                                                                              (if (syntax-case expr (#%app #%top)
                                                                                    [(#%app (#%top . var) . _)
                                                                                     (and foot-wrap?
                                                                                          (non-annotated-proc? (syntax var)))]
                                                                                    [else #f])
                                                                                  (return-value-wrap (d->so expr arg-temps))
                                                                                  (d->so expr arg-temps))))]
                                      [debug-info (make-debug-info-app new-tail-bound
                                                                       (varref-set-union (list free-varrefs arg-temps)) ; NB using bindings as vars
                                                                       'not-yet-called)]
                                      [let-body (wcm-wrap debug-info (d->so #f `(begin ,@set!-list ,final-app)))])
                                 (d->so #f `(let-values ,let-clauses ,let-body))))])
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
                   (d->so expr `(define-values (syntax vars-stx) ,annotated-val))
                   free-varrefs-val))]
               
               
               [(#%top . var)
                (let*-2vals ([annotated (syntax var)]
                             [free-varrefs (list (syntax var))])
                  (2vals 
                   (ccond [(or cheap-wrap? ankle-wrap?)
                           (appropriate-wrap annotated free-varrefs)]
                          [foot-wrap?
                           (wcm-break-wrap (make-debug-info-normal free-varrefs)
                                           (return-value-wrap annotated))])
                   free-varrefs))]
               
               ; this case must be last, because the pattern will match anything at all..
               [var
                (let*-2vals ([annotated (syntax var)]
                             [free-varrefs (list (syntax var))])
                  (2vals 
                   (ccond [(or cheap-wrap? ankle-wrap?)
                           (if (not (syntax-property (syntax var) 'never-undefined))
                               (appropriate-wrap annotated free-varrefs)
                               annotated)]
                          ; as far as I can tell, we can skip the result-break on lexical vars...
                          [foot-wrap? 
                           (wcm-break-wrap (make-debug-info-normal free-varrefs) annotated)])
                   free-varrefs))])))
         
         (define (annotate/top-level expr)
           (let-values ([(annotated dont-care)
                         (top-level-annotate/inner expr)])
             annotated)))
         
         ; body of local
      (let* ([annotated-expr (annotate/top-level expr)])
        ;(printf "annotated: ~n~a~n" (car annotated-exprs))
        (values annotated-expr struct-proc-names))))
  
)
