(require-library "core.ss")

#|
  (test-error <expression>)
  elaborates to 
  (with-handlers ([exn? (lambda (e) 
                             (printf "~e~n" (exn-message e))
                             #t)])
       ,<expression>)

|#
(define (test-error-transform form)
  `(with-handlers ([exn? (lambda (e) 
                           (printf "~a~n" (exn-message e))
                           #t)])
     ,form
     #f))

(define-macro test-error test-error-transform)

#| Tests: 
(equal? (test-error-transform '<expression>)
        '(with-handlers ([exn? (lambda (e) 
                                (printf "~a~n" (exn-message e))
                                #t)])
          <expression>
           #f))

|#