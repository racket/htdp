(module model-settings mzscheme
  (require "mred-extensions.ss"
           (prefix p: (lib "pconvert.ss"))
           "my-macros.ss"
           (lib "specs.ss" "framework"))
  
  (provide

   ; namespace queries
   check-global-defined
   global-lookup
   
   ; settings queries
   true-false-printed?
   constructor-style-printing?
   abbreviate-cons-as-list?
   ;special-function?
   
   ;print-convert
   print-convert)
  
  (define (true-false-printed?) (p:booleans-as-true/false))
  (define (constructor-style-printing?) (p:constructor-style-printing))
  (define (abbreviate-cons-as-list?) (p:abbreviate-cons-as-list))
  
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
  
   (define (print-convert val)
     (parameterize ([p:current-print-convert-hook
                     (lambda (v basic-convert sub-convert)
                       (if (image? v)
                           v
                           (basic-convert v)))])
       (p:print-convert val)))
   
  )