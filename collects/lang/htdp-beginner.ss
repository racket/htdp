
;; Implements the Beginner Scheme language, at least in terms of the
;; forms and procedures. The reader-level aspects of the language
;; (e.g., case-sensitivity) are not implemented here.

(module htdp-beginner mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "docprovide.ss" "syntax"))
  
  ;; Implements the forms:
  (require "private/teach.ss")
  
  ;; syntax:
  (provide (rename beginner-define define)
	   (rename beginner-define-struct define-struct)
	   (rename beginner-lambda lambda)
	   (rename beginner-app #%app)
	   (rename beginner-top #%top)
	   (rename beginner-cond cond)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
	   (rename beginner-quote quote)
	   (rename #%plain-module-begin #%module-begin)
	   #%datum
	   empty true false)
  
  (define-syntax (in-rator-position-only stx)
    (syntax-case stx ()
      [(_ new-name orig-name)
       (let ([new (syntax new-name)]
             [orig (syntax orig-name)])
         ;; Some things are not really functions:
         (if (memq (syntax-e orig) '(beginner:pi beginner:e beginner:null beginner:eof))
             #'(define new-name orig-name)
             #'(define-syntax (new-name stx)
                 (syntax-case stx ()
                   [(id . args)
                    (syntax (beginner-app orig-name . args))]
                   [_else
                    (raise-syntax-error
                     #f
                     (string-append
                      "this primitive operator must be applied to arguments; "
                      "expected an open parenthesis before the operator name")
                     stx)]))))]))
  
  ;; procedures:
  (provide-and-document/wrap
   procedures
   in-rator-position-only
   (all-from beginner: (lib "beginner-funs.ss" "lang" "private") procedures)))
