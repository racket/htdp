
(module intermediate mzscheme
  (require "private/teach.ss"
	   (lib "etc.ss")
	   (lib "list.ss")
	   (lib "docprovide.ss" "syntax"))

  ;; syntax:
  (provide (rename beginner-define define)
	   (rename beginner-define-struct define-struct)
	   (rename intermediate-lambda lambda)
	   (rename intermediate-app #%app)
	   (rename intermediate-local local)
	   (rename intermediate-let let)
	   (rename intermediate-letrec letrec)
	   (rename beginner-cond cond)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
	   (rename quote quote)
	   (rename intermediate-quasiquote quasiquote)
	   (rename intermediate-unquote unquote)
	   (rename intermediate-unquote-splicing unquote-splicing)
	   (rename intermediate-time time)
	   (rename #%plain-module-begin #%module-begin)
	   #%datum
	   #%top
	   empty true false)

  ;; procedures:
  (provide-and-document
   procedures
   (all-from beginner: (lib "beginner.ss" "lang") procedures)))
