(module shared mzscheme
  
  (require "my-macros.ss")
  (require "highlight-placeholder.ss")
  
  (provide
   (struct before-after-result (finished-exprs exp redex post-exp reduct after-exprs))
   (struct before-error-result (finished-exprs exp redex err-msg after-exprs))
   (struct error-result (finished-exprs err-msg))
   (struct finished-result (finished-exprs))
   list-take
   list-partition
   (struct closure-record (name mark constructor? lifted-name)) ; name field may be removed? depends on structs.
   *unevaluated* 
   no-sexp
   struct-flag
   multiple-highlight
   flatten-take
   closure-table-put!
   closure-table-lookup
   insert-highlighted-value
   get-lifted-var
   get-arg-var
   d->so
   syntax->ilist
   ilist-map
   ilist-flatten
   zip
   let-counter
   skipto
   ; get-binding-name
   ; bogus-binding?
   ; if-temp
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

  ; bogus-binding is used so that we can create legal bindings for temporary variables
  
  (define (create-bogus-binding name)
    (let* ([gensymed-name (gensym name)]
           [binding (datum->syntax-object #'here gensymed-name)])
      binding))
  
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
  
  
  ; get-arg-var maintains a list of bindings associated with the non-negative
  ; integers.  These symbols are used in the elaboration of applications; the nth
  ; in the application is evaluated and stored in a variable whose name is the nth
  ; gensym supplied by get-arg-var.
  
  (define get-arg-var
    (make-binding-source "arg" create-bogus-binding number->string))
  
  ; test cases: (returns #t on success)
;  (printf "test of get-arg-binding: ~a\n"
;          (let* ([arg3 (get-arg-var 3)]
;                 [arg2 (get-arg-var 2)]
;                 [arg1 (get-arg-var 1)]
;                 [arg2p (get-arg-var 2)])
;            (and (not (eq? arg3 arg2))
;                 (not (eq? arg3 arg1))
;                 (not (eq? arg3 arg2p))
;                 (not (eq? arg2 arg1))
;                 (eq? arg2 arg2p)
;                 (not (eq? arg1 arg2p)))))

  
  ; get-lifted-var maintains the mapping between let-bindings and the syntax object
  ; which is used to capture its index at runtime.
  ; unfortunately, it can't use "make-binding-source" because you need to compare the items 
  ; with module-variable=?, which means that hash tables won't work.
  
  ; my weak-assoc lists are lists of two-element lists, where the first one is in a weak box.
  ; furthermore, the whole thing is in a box, to allow it to be banged when needed.
  
  (define (weak-assoc-add boxed-lst key value)
       (set-box! boxed-lst (cons (list (make-weak-box key) value) (unbox boxed-lst))))
  
  (define (weak-assoc-search boxed-lst key eq-fun)
    (let* ([lst (unbox boxed-lst)]
           [found-val #f]
           [stripped (let loop ([remaining lst])
                       (if (null? remaining)
                           null
                           (let* ([first (car remaining)]
                                  [first-key (weak-box-value (car first))])
                             (if first-key
                                 (if (eq-fun key first-key)
                                     (begin 
                                       (set! found-val (cadr first))
                                       remaining)
                                     (cons first
                                           (loop (cdr remaining))))
                                 (loop (cdr remaining))))))])
      (set-box! boxed-lst stripped)
      found-val))
  
  ; test cases:
  ;  (define wa (box null))
  ;  (define-struct test ())
  ;  (weak-assoc-add wa 3 4)
  ;  (weak-assoc-add wa 9 10)
  ;  (= (weak-assoc-search wa 3 =) 4)
  ;  (= (weak-assoc-search wa 9 =) 10)
  ;  (= (weak-assoc-search wa 3 =) 4)
  ;  (= (length (unbox wa)) 2)
  ;  (define my-struct (make-test))
  ;  (weak-assoc-add wa my-struct 14)
  ;  (= (length (unbox wa)) 3)
  ;  (= (weak-assoc-search wa my-struct eq?) 14)
  ;  (set! my-struct #f)
  ;  (collect-garbage)
  ;  (= (length (unbox wa)) 3)
  ;  (= (weak-assoc-search wa 3 =) 4)
  ;  (= (length (unbox wa)) 2)
  
  (define lifted-index 0)
  (define (next-lifted-symbol str)
    (let ([index lifted-index]) 
      (set! lifted-index (+ lifted-index 1))
      (datum->syntax-object #'here (string->symbol (string-append str (number->string index))))))
 
  (define get-lifted-var
   (let ([assoc-table (box null)])
      (lambda (stx)
        (let ([maybe-fetch (weak-assoc-search assoc-table stx module-identifier=?)])
          (or maybe-fetch
              (begin
                (let* ([new-binding (next-lifted-symbol
                                     (string-append "lifter-" (format "~a" (syntax-object->datum stx)) "-"))])
                  (weak-assoc-add assoc-table stx new-binding)
                  new-binding)))))))

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
 
  ; struct-flag : uninterned symbol
  (define struct-flag (gensym "struct-flag-"))
  
  ; unit-result-values-list
  (define unit-result-values-list (gensym "unit-result-vaues-list"))
  
  ; list-partition takes a list and a number, and returns a 2vals containing 2 lists; the first one contains the
  ; first n elements of the list, and the second contains the remainder.  If n is greater than
  ; the length of the list, the exn:application:mismatch exception is raised.
  
  (define (list-partition lst n)
    (if (= n 0)
        (2vals null lst)
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
  
  ;; d->so uses a local syntax reference for the lexical context argument
  (define (d->so datum)
    (datum->syntax-object #'here datum))

  ;; ilist : for our puposes, an ilist is defined like this:
  ;; ILIST : (union null (cons ILIST-VAL ILIST) (cons ILIST-VAL ILIST-VAL))
  ;; ... where an ilist val can be anything _except_ a pair or null

  ;; syntax->ilist : turns an (possibly improper) syntax list into a (possibly) improper list of syntax objects

  (define (syntax->ilist stx-ilist)
    (let loop ([ilist (syntax-e stx-ilist)])
      (cond [(pair? ilist) 
             (unless (syntax? (car ilist))
               (error 'syntax->ilist "argument is not a syntax-ilist: ~a~n" stx-ilist))
             (cons (car ilist)
                   (loop (cdr ilist)))]
            [(null? ilist) null]
            [else
             (unless (syntax? ilist)
               (error 'syntax->ilist "argument is not a syntax-ilist: ~a~n" stx-ilist))
             (if (pair? (syntax-e ilist))
                 (loop (syntax-e ilist))
                 ilist)])))

  ; ilist-val? returns true if the value is neither a pair nor null
  (define (ilist-val? val)
    (not (or (null? val)
             (pair? val))))
  
  ; ilist-map applies the fn to every element of the ilist
  
  (define (ilist-map fn ilist)
    (let loop ([ilist ilist])
      (cond [(null? ilist)
             null]
            [(ilist-val? (cdr ilist))
             (cons (fn (car ilist)) (fn (cdr ilist)))]
            [else
             (cons (fn (car ilist)) (loop (cdr ilist)))])))
  
  ; ilist-flatten : produces a list containing the elements of the ilist
  (define (ilist-flatten ilist)
    (let loop ([ilist ilist])
      (cond [(null? ilist)
             null]
            [(ilist-val? (cdr ilist))
             (cons (car ilist) (cons (cdr ilist) null))]
            [else
             (cons (car ilist) (loop (cdr ilist)))])))
  
  ; zip : (listof 'a) (listof 'b) (listof 'c) ... -> (listof (list 'a 'b 'c ...))
  ; zip reshuffles lists of items into a list of item-lists.  Look at the contract, okay?
  
  (define zip
    (lambda args
      (apply map list args)))
  
  (define let-counter (syntax-property (d->so 'let-counter) 'stepper-binding-type 'stepper-temp))
  
  ;;   ;      ;                  ;               
 ;  ;  ;                                         
 ;     ;   ;  ;  ; ;;;   ; ;;;   ;  ; ;;    ;; ; 
 ;     ;  ;   ;  ;;   ;  ;;   ;  ;  ;;  ;  ;  ;; 
  ;;   ; ;    ;  ;    ;  ;    ;  ;  ;   ;  ;   ; 
    ;  ;;     ;  ;    ;  ;    ;  ;  ;   ;  ;   ; 
    ;  ; ;    ;  ;    ;  ;    ;  ;  ;   ;  ;   ; 
 ;  ;  ;  ;   ;  ;;   ;  ;;   ;  ;  ;   ;  ;  ;; 
  ;;   ;   ;  ;  ; ;;;   ; ;;;   ;  ;   ;   ;; ; 
                 ;       ;                     ; 
                 ;       ;                 ;;;;  
                                                 
 
  ; skipto : (listof number) SYNTAX-OBJECT (SYNTAX-OBJECT -> SYNTAX-OBJECT) -> SYNTAX-OBJECT
  ; skipto : opens up an existing syntax-object to a position indicated by the posn-list,
  ; then rebuilds the expression using the result of applying the annotater to the sub-term.
  ; the posn-list is used by converting the stx to a list, then recurring on the nth element,
  ; where n is indicated by the first number in the list.
  
(define skipto
  (checked-lambda (posn-list (stx SYNTAX-OBJECT) annotater)
    (if (null? posn-list)
        (annotater stx)
        (let ([opened (syntax->list stx)])
          (unless opened (error 'skipto "unable to apply syntax->list to ~a" (syntax-object->datum stx)))
          (datum->syntax-object 
           #'here
           (let loop ([iter (car posn-list)] [stx-list opened])
             (if (= iter 0)
                 (cons (skipto (cdr posn-list) (car stx-list) annotater)
                       (cdr stx-list))
                 (cons (car stx-list) 
                       (loop (- iter 1) (cdr stx-list)))))
           stx)))))
  
  ;test case
;  (and (equal? (syntax-object->datum (skipto '(0 2) #'((a b c) (d e f) (g h i)) (lambda (dc) #'foo)))
;               '((a b foo) (d e f) (g h i))))
                    
 
  )

; test cases
;(require shared)
;(load "/Users/clements/plt/tests/mzscheme/testing.ss")
;
;(define (a sym) 
;  (syntax-object->datum (get-lifted-var sym)))
;(define cd-stx 
;  (datum->syntax-object #f 'cd))
;(test 'lifter-ab-0  a (datum->syntax-object #f 'ab))
;(test 'lifter-cd-1 a cd-stx)
;(test 'lifter-ef-2 a (datum->syntax-object #f 'ef))
;(test 'lifter-cd-1 a cd-stx)
;
;(test '(a b c) map syntax-e (syntax->ilist #'(a b c)))
;(test '(a b c) map syntax-e (syntax->ilist #'(a . (b c))))
;(let ([result (syntax->ilist #' (a b . c))])
;  (test 'a syntax-e (car result))
;  (test 'b syntax-e (cadr result))
;  (test 'c syntax-e (cddr result)))
;
;(define (add1 x) (+ x 1))
;(test '(3 4 5) ilist-map add1 '(2 3 4))
;(test '(3 4 . 5) ilist-map add1 '(2 3 . 4))
;
;(test '(2 3 4) ilist-flatten '(2 3 4))
;(test '(2 3 4) ilist-flatten '(2 3 . 4))