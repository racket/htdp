
(module htdp-beginner-abbr mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "math.ss")
	   (lib "docprovide.ss" "syntax"))

  ;; Implements the forms:
  (require "private/teach.ss"
	   "private/teachprims.ss")

  ;; syntax:
  (provide (rename beginner-define define)
	   (rename beginner-define-struct define-struct)
	   (rename beginner-lambda lambda)
	   (rename beginner-app #%app)
	   (rename beginner-cond cond)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
	   (rename quote quote)
	   (rename intermediate-quasiquote quasiquote)
	   (rename intermediate-unquote unquote)
	   (rename intermediate-unquote-splicing unquote-splicing)
	   (rename #%plain-module-begin #%module-begin)
	   #%datum
	   #%top
	   empty true false)

  ;; procedures:
  (provide-and-document
   procedures
   (all-from beginner: (lib "htdp-beginner.ss" "lang") procedures)))
