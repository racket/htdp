#lang racket/base
(provide configure configure/settings
         options->sl-runtime-settings
         (struct-out sl-runtime-settings)
         sl-render-value/format)

(require mzlib/pconvert
         racket/pretty
         lang/private/set-result
         lang/private/rewrite-error-message
         (prefix-in image-core: mrlib/image-core)
         mrlib/cache-image-snip
         (only-in racket/draw bitmap%)
         racket/snip
         racket/class
         (only-in test-engine/test-markup get-rewritten-error-message-parameter render-value-parameter)
         (only-in test-engine/syntax report-signature-violation!)
         (only-in deinprogramm/signature/signature
                  signature? signature-name
                  signature-violation-proc)
         (only-in simple-tree-text-markup/construct number)
         simple-tree-text-markup/text
         "print-width.rkt")

(struct sl-runtime-settings
  (printing-style ; write, trad-write, print, quasiquote
   fraction-style ; mixed-fraction, mixed-fraction-e, repeating-decimal, repeating-decimal-e
   show-sharing?
   insert-newlines?
   tracing? ; unclear if this should be here
   true/false/empty-as-ids?
   abbreviate-cons-as-list?
   use-function-output-syntax?))

(define insert-newlines (make-parameter #t))

(define (options->sl-runtime-settings options)
  (sl-runtime-settings 'print
                       'repeating-decimal
                       (and (memq 'show-sharing options) #t)
                       #t ; insert-newlines?
                       #f ; tracing?
                       #f ; true/false/empty-as-ids?
                       (and (memq 'abbreviate-cons-as-list options) #t)
                       (and (memq 'use-function-output-syntax options) #t)))

(define (configure options)
  (configure/settings (options->sl-runtime-settings options)))

(define (configure/settings settings)
  ;; Set print-convert options:
  (booleans-as-true/false (sl-runtime-settings-true/false/empty-as-ids? settings))
  (print-boolean-long-form #t)
  [constructor-style-printing
   (case (sl-runtime-settings-printing-style settings)
     [(quasiquote) #f]
     [else #t])]
  (print-as-expression #f)
  (add-make-prefix-to-constructor #t)
  (abbreviate-cons-as-list (sl-runtime-settings-abbreviate-cons-as-list? settings))
  (insert-newlines (sl-runtime-settings-insert-newlines? settings))
  (current-print-convert-hook
   (let ([ph (current-print-convert-hook)])
     (lambda (val basic sub)
       (cond
         [(and (not (sl-runtime-settings-true/false/empty-as-ids? settings)) (equal? val '())) ''()]
         [(equal? val set!-result) '(void)]
         [(signature? val)
          (or (signature-name val)
              '<signature>)]
         [(is-image? val) val]
         [else (ph val basic sub)]))))
  (use-named/undefined-handler
   (lambda (x)
     (and (sl-runtime-settings-use-function-output-syntax? settings)
          (procedure? x)
          (object-name x))))
  (named/undefined-handler
   (lambda (x)
     (string->symbol
      (format "function:~a" (object-name x)))))

  ; sharing done by print-convert
  (show-sharing (sl-runtime-settings-show-sharing? settings))
  ; sharing done by write
  (print-graph (and (sl-runtime-settings-show-sharing? settings)
                    ;; print-convert takes care of this also, so only do it when that doesn't happen
                    (case (sl-runtime-settings-printing-style settings)
                      ([trad-write write] #t)
                      (else #f))))

  (define img-str "#<image>")
  (define (is-image? val)
    (or (is-a? val image-core:image%) ; 2htdp/image
        (is-a? val cache-image-snip%) ; htdp/image
        (is-a? val image-snip%) ; literal image constant
        (is-a? val bitmap%))) ; works in other places, so include it here too

  ;; exact fractions - slight hack as we know for what numbers DrRacket generates special snips
  (define (use-number-markup? x)
    (and (number? x)
         (exact? x)
         (real? x)
         (not (integer? x))))

  (define fraction-view
    (case (sl-runtime-settings-fraction-style settings)
      [(mixed-fraction mixed-fraction-e) 'mixed]
      [(repeating-decimal repeating-decimal-e) 'decimal]))

  (pretty-print-show-inexactness #t)
  (pretty-print-exact-as-decimal (eq? fraction-view 'decimal))

  (pretty-print-print-hook
   (let ([oh (pretty-print-print-hook)])
     (λ (val display? port)
       (cond
        [(and (not (port-writes-special? port))
              (is-image? val))
         (display img-str port)]
        [(and (use-number-markup? val)
              (port-writes-special? port))
         (write-special (number val #:exact-prefix 'never #:inexact-prefix 'always #:fraction-view fraction-view) port)]
        [(number? val)
         (display (number-markup->string val #:exact-prefix 'never #:inexact-prefix 'always #:fraction-view fraction-view) port)]
        [else
         (oh val display? port)]))))

  (pretty-print-size-hook
   (let ([oh (pretty-print-size-hook)])
     (λ (val display? port)
       (cond
         [(and (not (port-writes-special? port))
               (is-image? val))
          (string-length img-str)]
        [(and (use-number-markup? val)
              (port-writes-special? port))
         1]
        [(number? val)
         (string-length (number-markup->string val #:exact-prefix 'never #:inexact-prefix 'always #:fraction-view fraction-view))]
        [else
         (oh val display? port)]))))

  ; test-engine
  (get-rewritten-error-message-parameter get-rewriten-error-message)
  ; test-engine
  (render-value-parameter
   (lambda (value port)
     (parameterize ([print-value-columns 40])
       (print value port))))

  (error-display-handler
   (let ([o-d-h (error-display-handler)])
     (λ (msg exn)
       (define x (get-rewriten-error-message exn))
       (o-d-h x exn))))

  (global-port-print-handler
   (lambda (val port [depth 0])
     (define printing-style (sl-runtime-settings-printing-style settings))
     (define cols
       (if (exact-integer? (print-value-columns)) ;; print-value-columns takes precedence
           (print-value-columns)
           (htdp-print-columns)))

     (parameterize ([print-value-columns (if (eqv? cols 'infinity)
                                             +inf.0
                                             cols)]
                    [pretty-print-columns
                     (if (sl-runtime-settings-insert-newlines? settings)
                         cols
                         'infinity)])
       (let [(val (case printing-style
                    [(write trad-write) val]
                    [else (print-convert val)]))]
         (case printing-style
           [(print) (pretty-print val port depth)]
           [(write trad-write constructor) (pretty-write val port)]
           [(quasiquote) (pretty-write val port)])))))

  (signature-violation-proc
   (lambda (obj signature message blame)
     (report-signature-violation! obj signature message blame))))

(define (sl-render-value/format value port width)
  (parameterize ([print-value-columns (if (eq? width 'infinity)
                                          +inf.0
                                          width)])
    (print value port)
    (unless (insert-newlines)
      (newline port))))
