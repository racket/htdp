
(module advanced mzscheme
  (require "private/teach.ss")

  ;; syntax:
  (provide (rename advanced-define define)
	   (rename advanced-define-struct define-struct)
	   (rename advanced-lambda lambda)
	   (rename #%app #%app)
	   (rename intermediate-local local)
	   (rename advanced-let let)
	   (rename advanced-recur recur)
	   (rename intermediate-letrec letrec)
	   (rename beginner-cond cond)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
	   (rename quote quote)
	   (rename intermediate-time time)
	   (rename begin begin)
	   (rename set! set!)
	   (rename delay delay)
	   (rename #%module-begin #%plain-module-begin)))
