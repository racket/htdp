(module my-macros mzscheme

  ;;;;;;;;;;
  ;;
  ;;  ccond implementation
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
  ;;  make-contract-checker
  ;;
  ;;;;;;;;;;
  
  (provide make-contract-checker checked-lambda)
  
  (define (datum->syntax-object stx (string->symbol (string-append "contract-check-" 
                                                                                         (symbol->string (syntax-e (syntax name)))
                                                                                         "?"))))
           
  (define-syntaxes (make-contract-checker checked-lambda)
     
  (define-syntax (make-contract-checker stx)
    (syntax-case stx (make-contract-checker)
      [(make-contract-checker name pred)
       (identifier? (syntax name))
       (let* ([new-binding-name ])
         (with-syntax ([checker-name new-binding-name])
           (syntax/loc stx (define (checker-name arg var-name)
                             (unless (pred arg)
                               (error 'checker-name "contract violation: arg ~s with value ~a does not satisfy the ~s predicate"
                                      var-name arg 'name))))))]
      [else (error 'make-contract-checker "bad syntax in ~a" stx)]))
  
  (define-syntax (checked-lambda stx)
    (syntax-case stx (checked-lambda)
      [(checked-lambda bindings . bodies)
       (let* ([bindings (syntax->list (syntax bindings))]
              [raw-bindings (datum->syntax-object bindings (map (lambda (binding) 
                                                                  (if (pair? (syntax-e binding))
                                                                      (car (syntax-e binding))
                                                                      binding))
                                                                bindings))]
              [checked-bindings (filter (lambda (binding) (pair? (syntax-e binding))) bindings)]
              [contract-checks (map (lambda (stx)
                                      (with-syntax ([(var-name contract-name) stx])
                                        (let ([checker-name
                                        (syntax/loc
              
  
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
;(require my-macros)
;
;(= (2vals-first (2vals 3 4)) 3)
;(= (2vals-second (2vals 3 4)) 4)
;
;(= 
; (let*-2vals
;     ([a (2vals 3 4)]
;      [(b c) a])
;   a
;   c)
; 4)
;
;(make-contract-checker my-type (lambda (x) (= x 3)))
;
;(contract-check-my-type? 3 'second-arg)
;(contract-check-my-type? 14 'first-arg)
;
