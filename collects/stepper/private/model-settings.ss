(module model-settings mzscheme
  (require (lib "mred.ss" "mred")
           (prefix p: (lib "pconvert.ss")))
  
  (provide
   ; setup
   gather-eventspace-info
   
   ; namespace queries
   set-user-namespace!
   set-user-pre-defined-vars!
   check-pre-defined-var
   check-global-defined
   global-lookup
   
   ; settings queries
   true-false-printed?
   constructor-style-printing?
   abbreviate-cons-as-list?
   special-function?
   
   ; image?
   image?
   
   ;print-convert
   print-convert)
   
  (define user-pre-defined-vars #f)
  (define (set-user-pre-defined-vars! vars-list)
    (set! user-pre-defined-vars vars-list))
  
  (define user-namespace #f)
  (define (set-user-namespace! namespace)
    (set! user-namespace namespace))
  
  (define (gather-eventspace-info)
    (set! user-namespace (current-namespace))
    (set! user-pre-defined-vars (map car (make-global-value-list)))
    (set! user-vocabulary (d:basis:current-vocabulary))
    (set! par-true-false-printed (p:booleans-as-true/false))
    (set! par-constructor-style-printing (p:constructor-style-printing))
    (set! par-abbreviate-cons-as-list (p:abbreviate-cons-as-list))
    (set! par-special-functions (map (lambda (name) (list name (global-defined-value name)))
                                     special-function-names)))
  
  (define (check-pre-defined-var identifier)
    (memq identifier user-pre-defined-vars))
  
  
  (define (check-global-defined identifier)
    (with-handlers
        ([exn:variable? (lambda args #f)])
      (global-lookup identifier)
      #t))
  
  (define (global-lookup identifier)
    (parameterize ([current-namespace user-namespace])
      (global-defined-value identifier)))
  
   (define print-convert
     (parameterize ([p:current-print-convert-hook
                     (lambda (v basic-convert sub-convert)
                       (if (image? v)
                           v
                           (basic-convert v)))])
       (p:print-convert val)))
   