(module error mzscheme
  (require "error-sig.ss"
           (lib "unitsig.ss"))
  (provide errorU)
  
  (define errorU
    (unit/sig errorS
      (import)
      
    ;; check-arg : sym bool str str TST -> void
      (define (check-arg pname condition expected arg-posn given)
        (unless condition
          (error pname "expected <~a> as ~a argument, given: ~e"
                 expected arg-posn given)))
      
    ;; check-arity : sym num (list-of TST) -> void
      (define (check-arity name arg# args)
        (if (>= (length args) arg#)
            (void)
            (error name "expects at least ~a arguments, given ~e"
                   arg# (length args))))
      
    ;; check-proc :
    ;;   sym (... *->* ...) num (union sym str) (union sym str) -> void
      (define (check-proc proc f exp-arity arg# arg-err)
        (unless (procedure? f)
          (error proc "procedure expected as ~s argument; given ~e" arg# f))
        (unless (procedure-arity-includes? f exp-arity)
          (let ([arity-of-f (procedure-arity f)])
            (error proc "procedure of ~a expected as ~s argument; given procedure of ~a "
                   arg-err arg# 
                   (cond
                     [(number? arity-of-f)
                      (if (= arity-of-f 1)
                          (format "1 argument")
                          (format "~s arguments" arity-of-f))]
                     [(arity-at-least? arity-of-f) (format "at least ~s arguments" (arity-at-least-value arity-of-f))]
                     [else (format "multiple arities (~s)" arity-of-f)]))))))))
