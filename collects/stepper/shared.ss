(module shared mzscheme
  
  (require "my-macros.ss")
  
  (provide
   (struct before-after-result (finished-exprs exp redex post-exp reduct after-exprs))
   (struct before-error-result (finished-exprs exp redex err-msg after-exprs))
   (struct error-result (finished-exprs err-msg))
   (struct finished-result (finished-exprs))
   list-take
   list-partition
   (struct closure-record (name mark constructor? lifted-name))
   *unevaluated* 
   no-sexp
   struct-flag
   highlight-placeholder
   multiple-highlight
   flatten-take
   closure-table-put!
   closure-table-lookup
   insert-highlighted-value
   binding-indexer 
   binding-index-reset
   get-lifted-var
   ; get-binding-name
   ; bogus-binding?
   ; if-temp
   ; get-arg-binding
   ; get-lifted-gensym
   ; expr-read
   ; set-expr-read!
   )
  

  ; A step-result is either:
  ; (make-before-after-result finished-exprs exp redex reduct)
  ; or (make-before-error-result finished-exprs exp redex err-msg)
  ; or (make-error-result finished-exprs err-msg)
  ; or (make-finished-result finished-exprs)
  (define-struct before-after-result (finished-exprs exp redex post-exp reduct after-exprs))
  (define-struct before-error-result (finished-exprs exp redex err-msg after-exprs))
  (define-struct error-result (finished-exprs err-msg))
  (define-struct finished-result (finished-exprs))
  
  ; the closure record is placed in the closure table

  (define-struct closure-record (name mark constructor? lifted-name))

;  ; bogus-binding is used so that we can create legal zodiac bindings for temporary variables
;  
;  (define (create-bogus-binding name)
;    (let* ([gensymed-name (gensym name)]
;           [binding (z:make-lexical-binding 'bogus #f #f (z:make-empty-back-box) 
;                                            gensymed-name name)])
;      (set-new-binding-name! binding gensymed-name)
;      binding))
;  
;  (define (bogus-binding? binding)
;    (eq? (z:zodiac-origin binding) 'bogus))

  ; make-binding-source creates a pool of bindings, indexed by arbitrary keys. These bindings
  ; not eq? to any other bindings[*], but a client can always get the same binding by
  ; invoking the resulting procedure with the same key (numbers work well). make-binding-source
  ; also takes a string which will be part of the printed representation of the binding's
  ; name; this makes debugging easier.
  ; [*] actually, this is not true if you don't use a one-to-one function as the binding-maker
  ; make-gensym-source : (string -> (key -> binding))
  
  (define (make-binding-source id-string binding-maker key-displayer)
    (let ([assoc-table (make-hash-table 'weak)])
      (lambda (key)
        (let ([maybe-fetch (hash-table-get assoc-table key (lambda () #f))])
          (or maybe-fetch
              (begin
                (let* ([new-binding (binding-maker 
                                     (string-append id-string (key-displayer key) "-"))])
                  (hash-table-put! assoc-table key new-binding)
                  new-binding)))))))
  
  
  ; get-binding-name extracts the S-expression name for a binding. Zodiac
  ; creates a unique, gensym'd symbol for each binding, but the name is
  ; unreadable. Here, we create a new gensym, but the name of the generated
  ; symbol prints in the same way as the original symbol.
  
;  (define (get-binding-name binding)
;    (let ([name (lookup-new-binding-name binding)])
;      (or name
;	  (let* ([orig-name (z:binding-orig-name binding)]
;		 [name (string->uninterned-symbol (symbol->string orig-name))])
;	    (set-new-binding-name! binding name)
;	    name))))
;
;  (define-values (lookup-new-binding-name set-new-binding-name!)
;    (let-values ([(getter setter) (z:register-client 'new-name (lambda () #f))])
;      (values
;       (lambda (parsed) (getter (z:parsed-back parsed)))
;       (lambda (parsed n) (setter (z:parsed-back parsed) n)))))

  ; get-arg-binding maintains a list of bindings associated with the non-negative
  ; integers.  These symbols are used in the elaboration of applications; the nth
  ; in the application is evaluated and stored in a variable whose name is the nth
  ; gensym supplied by get-arg-symbol.
  
;  (define get-arg-binding
;    (make-binding-source "arg" create-bogus-binding number->string))
;  
  ; test cases: (returns #t on success)
;  (let ([arg3 (get-arg-symbol 3)]
;        [arg2 (get-arg-symbol 2)]
;        [arg1 (get-arg-symbol 1)]
;        [arg2p (get-arg-symbol 2)])
;    (and (not (eq? arg3 arg2))
;	 (not (eq? arg3 arg1))
;	 (not (eq? arg3 arg2p))
;         (not (eq? arg2 arg1))
;         (eq? arg2 arg2p)
;         (not (eq? arg1 arg2p)))

  
  ; get-lifted-var maintains the mapping between let-bindings and the syntax object
  ; which is used to capture its index at runtime.
  
  (define lifted-index 0)
  (define (next-lifted-symbol str)
    (let ([index lifted-index]) 
      (set! lifted-index (+ lifted-index 1))
      (datum->syntax-object #f (string->symbol (string-append str (number->string index))))))

  (define get-lifted-var
    (make-binding-source "lifter-" next-lifted-symbol (lambda (stx) (format "~a" (syntax-object->datum stx)))))

  
  ; gensyms needed by many modules:

  ; no-sexp is used to indicate no sexpression for display.
  ; e.g., on an error message, there's no sexp.
  (define no-sexp (gensym "no-sexp-"))

  ; multiple-highlight is used to indicate multiple highlighted expressions
  (define multiple-highlight (gensym "multiple-highlight-"))
  
  ; *unevaluated* is the value assigned to temps before they are evaluated. It's not a symbol so
  ; it won't need quoting in the source.  Bit of a hack, I know.
  (define-struct *unevaluated-struct* ())
  (define *unevaluated* (make-*unevaluated-struct*))
 
  ; if-temp : uninterned-symbol
;  (define if-temp (create-bogus-binding "if-temp-"))

  ; struct-flag : uninterned symbol
  (define struct-flag (gensym "struct-flag-"))
  
  ; highlight-placeholder : uninterned symbol
  (define highlight-placeholder (gensym "highlight-placeholder"))

  ; unit-result-values-list
  (define unit-result-values-list (gensym "unit-result-vaues-list"))
  
  ; list-partition takes a list and a number, and returns a 2vals containing 2 lists; the first one contains the
  ; first n elements of the list, and the second contains the remainder.  If n is greater than
  ; the length of the list, the exn:application:mismatch exception is raised.
  
  (define (list-partition lst n)
    (if (= n 0)
        (values null lst)
        (if (null? lst)
            (list-ref lst 0) ; cheap way to generate exception
            (let*-2vals ([(first rest) (list-partition (cdr lst) (- n 1))])
              (2vals (cons (car lst) first) rest)))))

  ; to perform source correlation, we use the 'register-client' ability of zodiac to
  ; add fields to parsed structures at runtime.
  
;  (define expr-read read-getter)
;  (define set-expr-read! read-setter)
  
  (define (list-take n a-list)
    (if (= n 0)
        null
        (cons (car a-list) (list-take (- n 1) (cdr a-list)))))
  
  (define (flatten-take n a-list)
    (apply append (list-take n a-list)))
  
  (define-values (closure-table-put! closure-table-lookup)
    (let ([closure-table (make-hash-table 'weak)])
      (values
       (lambda (key value)
	 (hash-table-put! closure-table key value)
	 key)                                  ; this return allows a run-time-optimization
       (lambda args ; key or key & failure-thunk
         (apply hash-table-get closure-table args)))))
 
  ; insert-highlighted-value : sexp sexp -> sexp
  ; replaces highlight-placeholder in the first sexp with the second sexp
  
  (define (insert-highlighted-value exp inserted)
    (let ([recur (lambda (exp) (insert-highlighted-value exp inserted))])
      (cond [(list? exp)
             (map recur exp)]
            [(vector? exp)
             (list->vector (map recur (vector->list exp)))]
            [(eq? exp highlight-placeholder)
             inserted]
            [else exp])))
  
  ; binding-indexer: (z:parsed -> integer)
  
  (define-values (binding-indexer binding-index-reset)
    (let ([counter 0])
      (values
       (lambda ()
         (begin0 counter (set! counter (+ counter 1))))
       (lambda ()
         (set! counter 0)))))
  
  

  )

; test cases
;(require shared)
;
;(define (a sym) 
;  (syntax-object->datum (get-lifted-var sym)))
;(define cd-stx 
;  (datum->syntax-object #f 'cd))
;(eq? (a (datum->syntax-object #f 'ab)) 'lifter-ab-0)
;(eq? (a cd-stx) 'lifter-cd-1)
;(eq? (a (datum->syntax-object #f 'ef)) 'lifter-ef-2)
;(eq? (a cd-stx) 'lifter-cd-1)