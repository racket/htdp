(module my-macros mzscheme

  (require-for-syntax (lib "list.ss"))
  
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
  
  (define-syntaxes (make-contract-checker checked-lambda)
    (let* ([make-checker-id 
            (lambda (name stx)
              (datum->syntax-object stx (string->symbol (string-append "contract-check-" 
                                                                       (symbol->string (syntax-e name))
                                                                       "?"))))]
           [make-contract-checker
            (lambda (stx)
              (syntax-case stx (make-contract-checker)
                [(make-contract-checker name pred)
                 (identifier? (syntax name))
                 (let* ([new-binding-name (make-checker-id (syntax name) stx)])
                   (with-syntax ([checker-name new-binding-name])
                     (syntax/loc stx (define (checker-name arg var-name)
                                       (unless (pred arg)
                                         (error 'checker-name "contract violation: arg ~s with value ~a does not satisfy the ~s predicate"
                                                var-name arg 'name))))))]
                [else (error 'make-contract-checker "bad syntax in ~a" stx)]))]
           [checked-lambda
            (lambda (stx)
              (syntax-case stx (checked-lambda)
                [(checked-lambda bindings . bodies)
                 (let* ([bindings-list (syntax->list (syntax bindings))]
                        [stripped-bindings (datum->syntax-object (syntax bindings)
                                                                 (map (lambda (binding) 
                                                                        (if (pair? (syntax-e binding))
                                                                            (car (syntax-e binding))
                                                                            binding))
                                                                      bindings-list))]
                        [checked-bindings (filter (lambda (binding) (pair? (syntax-e binding))) bindings-list)]
                        [contract-checks (map (lambda (pair-stx)
                                                (with-syntax ([(var-name contract-name) pair-stx])
                                                  (with-syntax ([checker-name (make-checker-id  (syntax contract-name) stx)])
                                                  (syntax/loc (car (syntax-e (syntax bodies))) (checker-name var-name 'var-name)))))
                                              checked-bindings)])
                   (datum->syntax-object stx `(lambda ,stripped-bindings ,@contract-checks ,@(syntax->list (syntax bodies)))))]
                [else (error 'checked-lambda "bad syntax in ~a" stx)]))])
      (values make-contract-checker checked-lambda)))
              
  
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
; (require my-macros)
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
;;(contract-check-my-type? 14 'first-arg)
;
;((checked-lambda (x (y my-type) (z my-type))
;    (+ x y z))
; 3 3 5)
;
