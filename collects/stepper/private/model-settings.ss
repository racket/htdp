(module model-settings mzscheme
  (require "mred-extensions.ss"
           (prefix p: (lib "pconvert.ss"))
           "my-macros.ss"
           (lib "specs.ss" "framework"))
  
  (provide

   ; namespace queries
   check-global-defined ; : (symbol -> boolean)
   global-lookup
   
   ; settings queries
   set-fake-beginner-mode ; : (boolean -> (void))
   
   true-false-printed? ; : ( -> boolean)
   constructor-style-printing? ; : ( -> boolean)
   abbreviate-cons-as-list? ; : ( -> boolean)
   ;special-function?
   
   print-convert)
  
  (define (set-fake-beginner-mode x) 
    (set! fake-beginner-mode x))
  (define fake-beginner-mode #f)
  
  (define (true-false-printed?) (or fake-beginner-mode (p:booleans-as-true/false)))
  (define (constructor-style-printing?) (or fake-beginner-mode (p:constructor-style-printing)))
  (define (abbreviate-cons-as-list?) (not (or fake-beginner-mode (not (p:abbreviate-cons-as-list)))))
  
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