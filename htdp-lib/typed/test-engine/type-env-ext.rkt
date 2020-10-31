#lang racket/base

(require typed-racket/utils/utils
         (prefix-in ce: test-engine/racket-tests)
         (for-syntax
          racket/base syntax/parse
          (utils tc-utils)
          (env init-envs)
          (rep filter-rep object-rep type-rep)
          (types abbrev numeric-tower union)))

(define-for-syntax ce-env
  (make-env
   ;; test*
   [(syntax-parse (local-expand #'(ce:test) 'expression null)
      #:context #'ce:test
      [(_ ce-t:id) #'ce-t])
    (-> -Void)]
   [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
      #:literals (define-values)
      [(define-values _
          (add-check-expect-test! _))
       #'add-check-expect-test!])
      ((-> Univ) . -> . -Void)]
   [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
      #:literals (define-values)
      [(define-values _
         (add-check-expect-test! (lambda () (do-check-expect _ _ _))))
       #'do-check-expect])
    ((-> Univ) Univ Univ . -> . -Boolean)]
   [(syntax-parse (local-expand #'(ce:check-within 1 1 1) 'module #f)
      #:literals (define-values)
      [(define-values _
         (add-check-expect-test! (lambda () (do-check-within _ _ _ _))))
       #'do-check-within])
    ((-> Univ) Univ -Real Univ . -> . -Boolean)]
   [(syntax-parse (local-expand #'(ce:check-error 1 "foo") 'module #f)
      #:literals (define-values)
      [(define-values _
         (add-check-expect-test! (lambda () (do-check-error _ _ _))))
       #'do-check-error])
    ((-> Univ) -String Univ . -> . -Boolean)]
   [(syntax-parse (local-expand #'(ce:check-range 1 1 1) 'module #f)
      #:literals (define-values)
      [(define-values _
         (add-check-expect-test! (lambda () (do-check-range _ _ _ _))))
       #'do-check-range])
    ((-> -Real) -Real -Real Univ . -> . -Boolean)]
   [(syntax-parse (local-expand #'(ce:check-member-of 1 1) 'module #f)
      #:literals (define-values)
      [(define-values _
         (add-check-expect-test! (lambda () (do-check-member-of _ _ _))))
       #'do-check-member-of])
    ((-> Univ) (-lst Univ) Univ . -> . -Boolean)]
   [(syntax-parse (local-expand #'(ce:check-random 1 1) 'module #f)
      #:literals (define-values)
      [(define-values _
         (add-check-expect-test! (lambda () (do-check-random _ _ _))))
       #'do-check-random])
    ((-> Univ) (-> Univ) Univ . -> . -Boolean)]))

(begin-for-syntax (initialize-type-env ce-env))
