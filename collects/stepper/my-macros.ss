(module my-macros mzscheme
  (provide ccond)
  
  ;; ideally, this macro should preserve source location information...
  
  (define-syntax (ccond stx)
    (syntax-case stx ()
      [(_ (question answer) ...)
       (syntax
	(cond
          (question answer) ...
          (else (error 'ccond "fell off end of cond expression"))))])))

