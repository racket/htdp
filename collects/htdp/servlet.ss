; Author: Paul Graunke
#cs(module servlet mzscheme
     (require (lib "servlet-primitives.ss" "web-server")
              (lib "servlet-helpers.ss" "web-server")
              (lib "servlet-sig.ss" "web-server")
              (lib "error.ss" "htdp")
              (lib "xml.ss" "xml")
              (lib "list.ss")
              (lib "prim.ss" "lang")
              (lib "unitsig.ss"))
     (provide (all-from-except (lib "servlet-sig.ss" "web-server") servlet^)
              (all-from-except (lib "servlet-helpers.ss" "web-server") build-suspender)
              (rename wrapped-build-suspender build-suspender))
     (provide-signature-elements servlet^)
     
     (define-values/invoke-unit/sig servlet^ servlet@ #f)
     
     ;  (define initial-request u:initial-request)
     ;  (define-primitive adjust-timeout! u:adjust-timeout!)
     ;  (define-primitive send/finish u:send/finish)
     ;  (define-higher-order-primitive send/suspend u:send/suspend (suspender))
     
     (define wrapped-build-suspender
       (case-lambda
         [(title content)
          (check-suspender2 title content)
          (build-suspender title content)]
         [(title content body-attributes)
          (check-suspender3 title content body-attributes)
          (build-suspender title content body-attributes)]
         [(title content body-attributes head-attributes)
          (check-suspender3 title content body-attributes head-attributes)
          (build-suspender title content body-attributes head-attributes)]))
     
     ; : tst tst -> void
     (define (check-suspender2 title content)
       (check-arg 'build-suspender (listof? xexpr? title) "(listof xexpr[HTML])" "1st" title)
       (check-arg 'build-suspender (listof? xexpr? content) "(listof xexpr[HTML])" "2nd" content))
     
     ; : tst tst tst -> void
     (define (check-suspender3 title content body-attributes)
       (check-suspender2 title content)
       (check-arg 'build-suspender (listof? attribute-pair? body-attributes)
                  "(listof (cons sym str))" "3rd" body-attributes))
     
     ; : tst tst tst tst -> void
     (define (check-suspender4 title content body-attributes head-attributes)
       (check-suspender3 title content body-attributes)
       (check-arg 'build-suspender (listof? attribute-pair? head-attributes)
                  "(listof (cons sym str))" "4th" head-attributes))
     
     ; : tst -> bool
     (define (xexpr? x)
       (or (string? x) (symbol? x) (number? x) (pcdata? x) (comment? x)
           (and (cons? x) (symbol? (car x))
                (or (and (cons? (cdr x)) (listof? xexpr-attribute? (cadr x))
                         (listof? xexpr? (cddr x)))
                    (listof? xexpr? (cdr x))))))
     
     ; : (a -> bool) tst -> bool
     ; To check if l is a (listof p?)
     ; Don't use (and (list? l) (andmap p? l)) because l may be improper.
     (define (listof? p? l)
       (let listof-p? ([l l])
         (or (null? l)
             (and (cons? l) (p? (car l)) (listof-p? (cdr l))))))
     
     ; : tst -> bool
     (define (xexpr-attribute? b)
       (and (pair? b) (symbol? (car b)) (pair? (cdr b)) (string? (cadr b)) (null? (cddr b))))
     
     ; : tst -> bool
     (define (attribute-pair? b)
       (and (pair? b) (symbol? (car b)) (string? (cdr b))))
     )
