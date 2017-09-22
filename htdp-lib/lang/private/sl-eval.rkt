#lang racket/base

(require teachpack/2htdp/scribblings/img-eval
         racket/pretty
         racket/sandbox
         mzlib/pconvert
         file/convertible
         scribble/eval)
(require lang/private/rewrite-error-message)

(provide
 ;; syntax: 
 ;; use with (define-module-local-eval e) ... (eval 'foo e)
 define-module-local-eval 

 ;; syntax: 
 ;; use with @interaction[#:eval (*sl-eval (define x ...) ...) ...] to create interactive examples 
 bsl-eval
 bsl+-eval
 isl-eval
 isl+-eval
 asl-eval)

;; this definition is a pile of hacks accumulated over the course of HtDP/2e writing 
;; there should be a better and simpler way to get this done
(require teachpack/2htdp/scribblings/img-eval)

(define-syntax *sl-eval
  (syntax-rules ()
    [(_ module-lang reader [def ...] exp ...)
     ;; ===>>>
     (let ()
       (define me (parameterize ([sandbox-propagate-exceptions #f])
                    (make-img-eval)))
       (me '(require (only-in racket empty? first rest cons? sqr true false)))
       (me '(require racket/pretty))
       (me '(require 2htdp/image))
       ;; see Matthew's email from 9 Feb 2016 to 'user'
       ;;       (me '(pretty-print-columns 15))
       ;;       (me '(current-print pretty-print-handler))
       (me '(current-print (lambda (v) (unless (void? v) (print v)))))
       (me 'def) ... ;; <--- too early ?
       (call-in-sandbox-context me (lambda () (error-print-source-location #f)))
       (call-in-sandbox-context me (lambda () (sandbox-output 'string)))
       (call-in-sandbox-context me (lambda () (sandbox-error-output 'string)))
       (call-in-sandbox-context me (lambda () (sandbox-propagate-exceptions #f)))
       (call-in-sandbox-context
        me
        (lambda ()
          (current-print-convert-hook
           (let ([prev (current-print-convert-hook)])
             ;; tell `print-convert' to leave images as themselves:
             (lambda (v basic sub)
               (if (convertible? v)
                   v
                   (prev v basic sub)))))
          
          (pretty-print-size-hook
           (let ([prev (pretty-print-size-hook)])
             ;; tell `pretty-print' that we'll handle images specially:
             (lambda (v w? op)
               (if (convertible? v) 1 (prev v w? op)))))
          
          (pretty-print-print-hook
           (let ([prev (pretty-print-print-hook)])
             ;; tell `pretty-print' how to handle images, which is
             ;; by using `write-special':
             (lambda (v w? op)
               (if (convertible? v) (write-special v op) (prev v w? op)))))
          
          (error-display-handler
           (lambda (msg exn)
             (cond
               [(exn? exn)
                (define msg (get-rewriten-error-message exn))
                (set! msg (regexp-replace #rx"  " msg " "))
                (set! msg (regexp-replace #rx": " msg ":"))
                (when (regexp-match #rx"but found" msg)
                  (set! msg (regexp-replace #rx"but found" msg "found")))
                (display msg (current-error-port))]
               [else (eprintf "uncaught exception: ~e" exn)])))
          
          ((dynamic-require 'htdp/bsl/runtime 'configure)
           (dynamic-require reader 'options))))
       ;; see Matthew's 9 Feb 2016 email to user
       (call-in-sandbox-context me (lambda () (namespace-require module-lang)))
       ; (me '(require mzlib/pconvert))
       ; (me '(add-make-prefix-to-constructor #t))
       (me 'exp) ...
       (interaction-eval #:eval me (require lang/posn))
       (interaction-eval #:eval me (require 2htdp/batch-io))
       me)]
    [(_ module-lang reader)
     (*sl-eval module-lang reader ())]))

(require lang/private/rewrite-error-message)

(define-syntax-rule
  (bsl-eval def ...)
  (*sl-eval 'lang/htdp-beginner 'htdp/bsl/lang/reader def ...))

(define-syntax-rule
  (bsl+-eval def ...)
  (*sl-eval 'lang/htdp-beginner-abbr 'htdp/bsl+/lang/reader def ...))

(define-syntax-rule
  (isl-eval def ...)
  (*sl-eval 'lang/htdp-intermediate 'htdp/isl/lang/reader def ...))

(define-syntax-rule 
  (isl+-eval def ...)
  (*sl-eval 'lang/htdp-intermediate-lambda 'htdp/isl/lang/reader def ...))

(define-syntax-rule 
  (asl-eval def ...)
  (*sl-eval 'lang/htdp-advanced 'htdp/asl/lang/reader def ...))

;; -----------------------------------------------------------------------------

;; (define-module-local-eval name-of-evaluator)
;; a make-base-eval whose namespace is initialized with the module where the macro is used 
(define-syntax-rule 
  (define-module-local-eval name)
  (begin
    (define-namespace-anchor ns)
    (define name 
      (parameterize ([sandbox-namespace-specs (list (lambda () (namespace-anchor->namespace ns)))]
                     [sandbox-error-output 'string]
                     [sandbox-output 'string])
        (make-base-eval)))))
