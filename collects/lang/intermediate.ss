
(module intermediate mzscheme
  (require "private/teach.ss")

  ;; syntax:
  (provide (rename beginner-define define)
	   (rename beginner-define-struct define-struct)
	   (rename beginner-lambda lambda)
	   (rename beginner-app #%app)
	   (rename intermediate-local local)
	   (rename intermediate-let let)
	   (rename intermediate-letrec letrec)
	   (rename beginner-cond cond)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
	   (rename beginner-quote quote)
	   (rename #%module-begin #%plain-module-begin)))
