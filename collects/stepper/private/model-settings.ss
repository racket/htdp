(module model-settings mzscheme
  (require "mred-extensions.ss"
           "my-macros.ss"
           (lib "specs.ss" "framework")
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
  
  (provide

   ; namespace queries
   check-global-defined ; : (symbol -> boolean)
   global-lookup
   
   get-render-settings ; (-> render-settings?)
   fake-beginner-render-settings ; render-settings?
   fake-mz-render-settings ; render-settings?
   
   render-settings? ; predicate
   
   set-render-to-string! ; (-> (-> any? string?) void?)
   set-render-to-sexp! ; (-> (-> (-> any) any) void?)
   )
  
  ; contracts for procedures in the settings unit:
  ; true-false-printed? ; : ( -> boolean)
  ; constructor-style-printing? ; : ( -> boolean)
  ; abbreviate-cons-as-list? ; : ( -> boolean)
  ; render-to-sexp ; (TST -> TST)
  (define render-settings? (vectorof/n procedure? procedure? procedure? procedure?))
  
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
  
  (define (fake-beginner-render-to-sexp val)
    (parameterize ([booleans-as-true/false #t]
                   [constructor-style-printing #t]
                   [abbreviate-cons-as-list #f])
      (print-convert val)))
    
  (define fake-beginner-render-settings
    (contract
     render-settings?
     (vector (lambda () #t) (lambda () #t) (lambda () #f) fake-beginner-render-to-sexp)
     'model-settings
     'caller))
  
  (define fake-mz-render-settings
    (contract
     render-settings?
     (vector booleans-as-true/false constructor-style-printing abbreviate-cons-as-list print-convert)
     'model-settings
     'caller))
  
  (define-struct test-struct ())
  
  (define get-render-settings
    (contract
     (-> render-settings?)
     (lambda ()
       (fprintf (current-error-port)
                "true : ~e\n(make-test-struct) : ~e\n(substring (render-to-string '(3)) 0 5) : ~e\n"
                (string=? (render-to-string #t) "true")
                (render-to-string (make-test-struct))
                (substring (render-to-string '(3)) 0 5))
       (let* ([true-false-printed/bool (string=? (render-to-string #t) "true")]
              [constructor-style-printing/bool (string=? (render-to-string (make-test-struct)) "(make-test-struct)")]
              [abbreviate-cons-as-list/bool (and constructor-style-printing/bool
                                                 (string=? (substring (render-to-string '(3)) 0 5) "(list"))])
         (vector
          (lambda () true-false-printed/bool)
          (lambda () constructor-style-printing/bool)
          (lambda () abbreviate-cons-as-list/bool)
          render-to-sexp)))
     'model-settings
     'caller))
  
  (define check-global-defined
    (contract
     (-> symbol? boolean?)
     (lambda (identifier)
       (with-handlers
           ([exn:variable? (lambda args #f)])
         (global-lookup identifier)
         #t))
     'model-settings
     'caller))
  
  (define global-lookup
    (contract
     (-> symbol? any)
     (lambda (identifier)
       (namespace-variable-value identifier))
     'model-settings
     'caller))
  )