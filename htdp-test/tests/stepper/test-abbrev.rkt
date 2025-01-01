#lang racket

(require (for-syntax scheme/mpair racket/list))

(provide t)

;; t : eli's convenient short-cut syntactic form for defining tests.

;; here's an example of how you might use it:
#;(let* ([defs1 `((define (a x) (+ x 5)) (define b a))]
       [defs2 (append defs1 `((define c a)))])
  (t 'top-ref4 m:intermediate
     ,@defs1 (define c b) (c 3)
     :: ,@defs1 (define c {b})
     -> ,@defs1 (define c {a})
     :: ,@defs2 ({c} 3)
     -> ,@defs2 ({a} 3)
     :: ,@defs2 {(a 3)}
     -> ,@defs2 {(+ 3 5)}
     -> ,@defs2 {8}))



(define-syntax (t stx)
  (define (maybe-mlist->list r)
    (if (mpair? r) 
        (mlist->list r)
        r))
  (define (split l)
    (let loop ([l l] [r '()])
      (cond [(null? l) (reverse (map maybe-mlist->list r))]
            [(symbol? (car l)) (loop (cdr l) (cons (car l) r))]
            [(or (null? r) (not (mpair? (car r))))
             (loop (cdr l) (cons (mlist (car l)) r))]
            [else (mappend! (car r) (mlist (car l))) 
                  (loop (cdr l) r)])))
  (define (process-hilites s)
    (syntax-case s ()
      [(x) (eq? #\{ (syntax-property s 'paren-shape))
           (with-syntax ([x (process-hilites #'x)]) #'(hilite x))]
      [(x . y) (let* ([x0 #'x]
                      [y0 #'y]
                      [x1 (process-hilites #'x)]
                      [y1 (process-hilites #'y)])
                 (if (and (eq? x0 x1) (eq? y0 y1))
                     s
                     (with-syntax ([x x1] [y y1]) #'(x . y))))]
      [_else s]))
  (define (process stx)
    (split (map (lambda (s)
                  (if (and (identifier? s)
                           (memq (syntax-e s) '(:: ::* -> error:)))
                      (syntax-e s)
                      (process-hilites s)))
                (syntax->list stx))))
  (define (parse-:: rest)
    (syntax-case rest (:: ::* -> error:)
      [(error: (err)) (list #'(error err))]
      [() (list #'(finished-stepping))]
      [(x -> y) (list #'(before-after x y) #'(finished-stepping))]
      [(x -> error: (err)) (list #'(before-error x err))]
      [(x -> y :: . rest)
       (cons #'(before-after x y) (parse-:: #'rest))]
      [(x -> y ::*([lo hi] . snd) . rest)
       (cons #'(before-after x y) (parse-::* #'lo #'hi #'(snd . rest)))]
      [(x -> y -> . rest)
       (cons #'(before-after x y) (parse-:: #'(y -> . rest)))]))
  (define (parse-::* lo hi curr)
    (define-values (prefix suffix)
      (let rep-scan-loop ([prevs '()] [curr curr])
        (syntax-case curr (:: ::*)
          [()           (values (reverse prevs) curr)]
          [(:: . rest)  (values (reverse prevs) curr)]
          [(::* . rest) (values (reverse prevs) curr)]
          [(fst . rest) (rep-scan-loop (cons #'fst prevs) #'rest)])))
    (cons #`(repetition #,lo #,hi #,(drop-right (parse-:: prefix) 1))
          ;; ^ drop-right to remove the trailing (finished-stepping) in repetition steps
          (syntax-case suffix (:: ::*)
            [()                                (parse-:: suffix)]
            [(:: rest ...)                     (parse-:: #'(rest ...))]
            [(::* ([lo hi] snd ...) rest ...)  (parse-::* #'lo #'hi #'((snd ...) rest ...))])))
  (define (parse l)
    (syntax-case l (:: ::*)
      [(fst :: rest ...)
       (cons #'fst (parse-:: #'(rest ...)))]
      [(fst ::* ([lo hi] snd ...) rest ...)
       (cons #'fst (parse-::* #'lo #'hi #'((snd ...) rest ...)))]))
  (syntax-case stx (:: ::*)
    [(_ name ll-models . rest)
     (with-syntax ([(exprs arg ...) (parse (process #'rest))])
       (quasisyntax/loc stx
         (list name
               ll-models
               ;printf "exprs = ~s\n args = ~s\n"
               (exprs->string `exprs) `(arg ...)
               '())))]))


;; (-> (listof sexp) string?)
(define (exprs->string exprs)
  (apply string-append
         (cdr (apply append (map (lambda (x) (list " " (format "~s" x)))
                                 exprs)))))
