(module model-settings mzscheme
  (require "mred-extensions.ss"
           "my-macros.ss"
           (lib "specs.ss" "framework")
           "pconvert.ss")
  
  ; there are two separate reasons to use units here, but it's just too painful.
  ; reason 1) the drscheme:language procedures are linked at runtime into the
  ; stepper-tool unit, and we need to get them down here somehow.  Making the
  ; render-settings a unit would solve this.
  ; reason 2) the render-settings should be recomputed once for each stepper 
  ; invocation.  invoke-unit is a nice way of doing this without dropping back
  ; to linking-by-position, which is what happens with a simple closure 
  ; implementatian.
  
  ; HOWEVER, like I said, it's just too painful. Once this is a unit, then 
  ; everything else wants to be a unit too. Yecch.  I'm not explaining this
  ; well, I know.
  
  (provide

   ; namespace queries
   check-global-defined ; : (symbol -> boolean)
   global-lookup
   
   print-convert ; (-> any any)
   
   get-render-settings ; (-> render-settings?)
   fake-beginner-render-settings ; render-settings?
   
   render-settings? ; predicate
   
   set-render! ; (-> (-> any? string?) void?)
   set-set-print-settings ; (-> (-> (-> any) any) void?)
   )
  
  ; contracts for procedures in the settings unit:
  ; true-false-printed? ; : ( -> boolean)
  ; constructor-style-printing? ; : ( -> boolean)
  ; abbreviate-cons-as-list? ; : ( -> boolean)
  (define proposition-contract (-> boolean?))
  (define render-settings? (vectorof/n proposition-contract proposition-contract proposition-contract))
  
  (define (render val)
    (error 'model-settings "render not set yet"))
  (define (set-print-settings val)
    (error 'model-settings "set-print-settings not set yet"))
  
  (define set-render!
    (contract
     (-> (-> any? string?) void?)
     (lx (set! render _))
     'model-settings
     'caller))
  
  (define set-set-print-settings!
    (contract
     (-> (-> (-> any)
             any)
         void?)
     (lx (set! set-print-settings _))
     'model-settings
     'caller))
  
  (define fake-beginner-render-settings
    (contract
     render-settings?
     (vector (lx #t) (lx #t) (lx #f))
     'model-settings
     'caller))
  
  (define-struct test-struct ())
  
  (define get-render-settings
    (contract
     (-> render-settings?)
     (lambda ()
       (let* ([true-false-printed/bool (string=? (render #t) "true")]
              [constructor-style-printing/bool (string=? (render (make-test-struct)) "(make-test-struct)")]
              [abbreviate-cons-as-list/bool (and constructor-style-printing/bool
                                                 (string=? (substring (render '(3)) 0 5) "(cons"))])
        (vector
         (lx true-false-printed/bool)
         (lx constructor-style-printing/bool)
         (lx abbreviate-cons-as-list/bool))))
     'model-settings
     'caller))
  
  ;; print-convert : (-> any any)
  (define print-convert
    (contract
     (-> any any)
     (lambda (val)
       (set-print-settings
        (lambda ()
          (simple-module-based-language 
      
  ;; COPIED FROM drscheme/private/language.ss
  ;; simple-module-based-language-convert-value : TST settings -> TST
  (define (simple-module-based-language-convert-value value settings)
        (case (simple-settings-printing-style settings)
          [(write) value]
          [(constructor)
           (parameterize ([constructor-style-printing #t]
                          [show-sharing (simple-settings-show-sharing settings)])
             (print-convert value))]
          [(quasiquote)
           (parameterize ([constructor-style-printing #f]
                          [show-sharing (simple-settings-show-sharing settings)])
             (print-convert value))]))
  
  
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
     (-> symbol? (lambda (x) #t))
     (lambda (identifier)
       (namespace-variable-value identifier))
     'model-settings
     'caller))
  )