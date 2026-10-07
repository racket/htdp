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
    [(drscheme:opt-out-toolbar-buttons)
     (append (if (memq 'enable-debugger options)
                 '()
                 '(debug-tool))
             '(macro-stepper))]

    [(drracket:opt-in-toolbar-buttons)
     (cond
       [(memq 'disable-stepper options) '()]
       [(and (member 'abbreviate-cons-as-list options)
             (member 'use-function-output-syntax options)
             (member 'read-accept-quasiquote options))
        (list 'htdp:stepper:isl+)]
       [(and (member 'abbreviate-cons-as-list options)
             (member 'read-accept-quasiquote options))
        (list 'htdp:stepper:bsl+)]
       [else
        (list 'htdp:stepper:bsl)])]
    
    [(drracket:show-big-defs/ints-labels) #t]

    [(documentation-language-family) "HtDP"]

    [(drracket:default-instrumentation) 'test-coverage]
    
    [else (use-default key default)]))
