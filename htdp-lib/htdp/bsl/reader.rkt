#lang scheme/base
(require (rename-in syntax/module-reader
                    [#%module-begin #%reader-module-begin]))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scheme/base)
                     #%module-begin))

(define-syntax-rule (module-begin lang opts)
  (#%reader-module-begin
   lang

   #:read (wrap-reader read options)
   #:read-syntax (wrap-reader read-syntax options)
   #:info (make-info options)

   (provide options)
   (define options opts)))

(define (wrap-reader read-proc options)
  (lambda args
    (parameterize ([read-decimal-as-inexact #f]
                   [read-accept-dot #f]
                   [read-accept-quasiquote (memq 'read-accept-quasiquote options)])
      (apply read-proc args))))

(define ((make-info options) key default use-default)
  (case key
    [(drscheme:toolbar-buttons)
     ;; if you want to add more buttons, the strategy used here might not
     ;; be the right one to use. If you can simply enable existing buttons,
     ;; do that instead.
     (append (if (memq 'disable-stepper options)
                 '()
                 (list ((dynamic-require 'lang/private/sl-stepper-button 'sl-stepper-drracket-button) options)))
             (list (dynamic-require 'drracket/syncheck-drracket-button 'syncheck-drracket-button)))]

    [(drscheme:opt-out-toolbar-buttons)
     (append (if (memq 'enable-debugger options)
                 '()
                 '(debug-tool))
             '(macro-stepper drracket:syncheck))]
    
    [(drracket:show-big-defs/ints-labels) #t]
    
    [else (use-default key default)]))
