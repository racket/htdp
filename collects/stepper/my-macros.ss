(module my-macros mzscheme

  ;;;;;;;;;;
  ;;
  ;;  2vals implementation
  ;; 
  ;;;;;;;;;;
  
(provide ccond)
  
  (define-syntax (ccond stx)
    (syntax-case stx ()
      [(_ (question answer) ...)
       (syntax
	(cond
          (question answer) ...
          (else (error 'ccond "fell off end of cond expression"))))]))
  
  
  ;;;;;;;;;;
  ;;
  ;;  2vals implementation
  ;; 
  ;;;;;;;;;;
  
  (provide 2vals let*-2vals 2vals-first 2vals-second)
  
  (define 2vals vector)
  
  (define-syntax (let*-2vals stx)
    (syntax-case stx (let*-2vals)
      [(let*-2vals () . bodies)
       (syntax (begin . bodies))]
      [(let*-2vals ([(id-a id-b) rhs] binding ...) . bodies)  ; 2 values in a vector
       (syntax (let* ([_a rhs] [id-a (vector-ref _a 0)] [id-b (vector-ref _a 1)])
                 (let*-2vals (binding ...) . bodies)))]
      [(let*-2vals ([id-a rhs] binding ...) . bodies)         ; just 1 value
       (syntax (let* ([id-a rhs]) 
                 (let*-2vals (binding ...) . bodies)))]))
  
  (define-syntax (2vals-first stx)
    (syntax-case stx (2vals-first)
      [(2vals-first a)
       (syntax (vector-ref a 0))]))
  
  (define-syntax (2vals-second stx)
    (syntax-case stx (2vals-second)
      [(2vals-second a)
       (syntax (vector-ref a 1))])))

; test cases
(require my-macros)

(= (2vals-first (2vals 3 4)) 3)
(= (2vals-second (2vals 3 4)) 4)

(= 
 (let*-2vals
     ([a (2vals 3 4)]
      [(b c) a])
   a
   c)
 4)


