#lang racket
(require rackunit)

(define (run-program str #:repl-interaction [repl-interaction #f])
  (parameterize ([current-namespace (make-base-namespace)])
    (define stx
      (parameterize ([read-accept-reader #t])
        (read-syntax "x.rkt" (open-input-string str))))
    (parameterize ([current-module-declare-name (make-resolved-module-path 'x)]
                   [current-module-declare-source 'x])
      (eval stx))
    (define op (open-output-string))
    (parameterize ([current-output-port op])
      (dynamic-require ''x #f))
    (when repl-interaction
      (parameterize ([current-namespace (module->namespace ''x)])
        (dynamic-require `(submod 'x configure-runtime) #f)
        (parameterize ([current-output-port op])
          (define v ((current-read-interaction) "interactions" (open-input-string repl-interaction)))
          (define w (cons '#%top-interaction v))
          (call-with-values
           (λ ()
             (eval-syntax
              (if (syntax? v)
                  (namespace-syntax-introduce
                   (datum->syntax #f w v))
                  v)))
           (λ lst
             (for ([x (in-list lst)])
               ((current-print) x)))))))
    (get-output-string op)))

(check-equal? (run-program "#lang htdp/bsl 1.2") "6/5\n")
(check-equal? (run-program "#lang htdp/bsl" #:repl-interaction "1.2") "1.2\n")
(check-equal? (run-program "#lang htdp/bsl" #:repl-interaction "(exact? 1.2)") "#true\n")

(check-equal? (run-program "#lang htdp/isl (exact? 1.2)") "#true\n")
(check-equal? (run-program "#lang htdp/isl" #:repl-interaction "1.2") "1.2\n")
(check-equal? (run-program "#lang htdp/isl" #:repl-interaction "(exact? 1.2)") "#true\n")

(check-equal? (run-program "#lang htdp/asl (exact? 1.2)") "#true\n")
(check-equal? (run-program "#lang htdp/asl" #:repl-interaction "1.2") "1.2\n")
(check-equal? (run-program "#lang htdp/asl" #:repl-interaction "(exact? 1.2)") "#true\n")

