(module model-settings mzscheme
  (require "mred-extensions.ss"
           "my-macros.ss"
           (lib "contracts.ss")
           (lib "pconvert.ss"))
  
  ; there are two separate reasons to use units here, but it's just too painful.
  ; reason 1) the drscheme:language procedures are linked at runtime into the
  ; stepper-tool unit, and we need to get them down here somehow.  Making the
  ; render-settings a unit would solve this.
  ; reason 2) the render-settings should be recomputed once for each stepper 
  ; invocation.  invoke-unit is a nice way of doing this without dropping back
  ; to linking-by-position, which is what happens with a simple closure 
  ; implementatian.
  
  ; HOWEVER, like I said, it's just too painful. Once this is a unit, then 
  ; everything else wants to be a unit too. For instance, ta make sure that 
  ; the reconstructor gets the right invocation of the unit, it needs to be a 
  ; unit as well.  Pretty soon, everything is units.

  ; contracts for procedures in the settings unit:
  ; true-false-printed? ; : ( -> boolean)
  ; constructor-style-printing? ; : ( -> boolean)
  ; abbreviate-cons-as-list? ; : ( -> boolean)
  ; render-to-sexp ; (TST -> TST)
  (define render-settings? (vector/p procedure? procedure? procedure? procedure?))

  (provide/contract [check-global-defined (-> symbol? boolean?)]
                    [global-lookup (-> any? any)]

                    [render-settings? contract?]
                    [get-render-settings (-> render-settings?)]
                    [set-render-to-string! (-> (-> any? string?) void?)]
                    [set-render-to-sexp! (-> (-> (-> any) any) void?)]
                    
                    [fake-beginner-render-settings render-settings?]
                    [fake-beginner-wla-render-settings render-settings?]
                    [fake-intermediate-render-settings render-settings?]
                    [fake-intermediate/lambda-render-settings render-settings?]
                    [fake-mz-render-settings render-settings?])
  

  (define (render-to-string val)
    (error 'model-settings "render not set yet"))
  (define (render-to-sexp val)
    (error 'model-settings "set-print-settings not set yet"))
  
  (define set-render-to-string!
    (contract
     (-> (-> any? string?) void?)
     (lx (set! render-to-string _))
     'model-settings
     'caller))
  
  (define set-render-to-sexp!
    (contract
     (-> (-> any? any)
         void?)
     (lx (set! render-to-sexp _))
     'model-settings
     'caller))
  
  (define (make-fake-render-to-sexp true/false constructor-style abbreviate)
    (lambda (val)
      (parameterize ([booleans-as-true/false true/false]
                     [constructor-style-printing constructor-style]
                     [abbreviate-cons-as-list abbreviate])
        ;; duplication of hack inserted by matthew flatt for 201 release:
        (or (and (procedure? val)
                 (object-name val))
            (print-convert val)))))
    
  (define (lt) #t)
  (define (lf) #f)
  
  (define fake-beginner-render-settings
    (vector lt lt lf (make-fake-render-to-sexp #t #t #f)))
  
  (define fake-beginner-wla-render-settings
    (vector lt lt lt (make-fake-render-to-sexp #t #t #t)))
  
  (define fake-intermediate-render-settings
    fake-beginner-wla-render-settings)
  
  (define fake-intermediate/lambda-render-settings
    fake-beginner-wla-render-settings)
  
  (define fake-mz-render-settings
    (vector booleans-as-true/false constructor-style-printing abbreviate-cons-as-list print-convert))
  
  (define-struct test-struct () (make-inspector))
  
  (define (get-render-settings)
    (let* ([true-false-printed/bool (string=? (render-to-string #t) "true")]
           [constructor-style-printing/bool (string=? (render-to-string (make-test-struct)) "(make-test-struct)")]
           [rendered-list (render-to-string '(3))]
           [rendered-list-substring (substring rendered-list 
                                               0 
                                               (min 5 (string-length rendered-list)))]
           [abbreviate-cons-as-list/bool (and constructor-style-printing/bool
                                              (string=? rendered-list-substring "(list"))])
      (vector
       (lambda () true-false-printed/bool)
       (lambda () constructor-style-printing/bool)
       (lambda () abbreviate-cons-as-list/bool)
       render-to-sexp)))
  
  (define (check-global-defined identifier)
    (with-handlers
        ([exn:variable? (lambda args #f)])
      (global-lookup identifier)
      #t))
  
  (define (global-lookup identifier)
    (namespace-variable-value identifier)))