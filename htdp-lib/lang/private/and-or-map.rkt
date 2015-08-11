#lang scheme

;; ---------------------------------------------------------------------------------------------------
;; define higher-order primitives that consume boolean-valued functions 

(require "teachprims.rkt" "teach.rkt")

(provide intermediate-andmap intermediate-ormap intermediate-filter 
         intermediate-quicksort intermediate-sort)


;; --- checking auxiliaries ---
(define (arity-check t r f a)
  (unless (and (procedure? f) (procedure-arity-includes? f a))
    (if (= a 1)
        (hocheck t "~a argument must be a function that accepts one argument, given ~e" r f)
        (hocheck t "~a argument must be a function that accepts ~a arguments, given ~e" r a f))))

(define-syntax-rule
  (boolean-test-wrapper tag (f . z))
  (let ()
    (define msg (format "the function given to ~a" tag))
    (define *name (object-name f))
    (define name (if *name (format "~a (~a)" *name msg) msg))
    (define (g . z)
      (define f@x (apply f z))
      (if (boolean? f@x)
          f@x
          (error tag "expected a boolean from ~a, but received ~e" name f@x)))
    g))

(define (list-check? name msg l)
  (unless (beginner-list? l) 
    (hocheck name "expected a list for the ~a argument, given ~e" msg l)))

;; --- refined function definitions --- 

(define-syntax define-boolean
  (syntax-rules ()
    [(define-boolean name)
     (define-teach intermediate name
       (lambda (f . l)
         (arity-check 'name "first" f (length l))
         (for/and ([l l] [i (in-naturals)])
                  (list-check? 'name (format "~ath" (+ i 2)) l)
           (unless (beginner-list? l) 
             (hocheck 'name (format "expected a list for the ~a argument, given ~e" (+ i 2)) l)))
         (define g (boolean-test-wrapper 'name (f . x)))
         (apply name g l)))]
    [(define-boolean name A)
     (define-teach intermediate name
       (lambda (f . l)
         (unless (= (length l) A)
           (error 'name "expected only ~a arguments, found ~a" (+ A 1) (+ (length l) 1)))
         ;; MF: stupid code replication, worry later 
         (arity-check 'name "first" f (length l))
         (for/and ([l l] [i (in-naturals)])
                  (list-check? 'name (format "~ath" (+ i 2)) l)
           (unless (beginner-list? l) 
             (hocheck 'name (format "expected a list for the ~a argument, given ~e" (+ i 2)) l)))
         (define g (boolean-test-wrapper 'name (f . x)))
         (apply name g l)))]))

(define-boolean andmap)
(define-boolean ormap)
(define-boolean filter 1)

(define-syntax-rule
  (define-sort name)
  (define-teach intermediate name 
    (lambda (l cmp?)
      (list-check? 'name "first" l)
      (arity-check 'name "second" cmp? 2)
      (define dmp? (boolean-test-wrapper 'name (cmp? . x)))
      (sort l dmp?))))

(define-sort sort)
(define-sort quicksort)

#| TESTS

(define (tester tag name)
  (lambda (x) 
    (unless (regexp-match name (exn-message x))
      (displayln '****broken-test****)
      (displayln `(,tag ,(exn-message x))))))

(define-syntax-rule 
  (exn-tester label name function inputs ...)
  (with-handlers ((exn:fail? (tester label name)))
    (function inputs ...)
    (error 'exn-tester "***case ~a (~a) failed***" label name)))

(exn-tester 1 "ormap" intermediate-ormap cons '(1 2 3))
(exn-tester 2 "andmap" intermediate-andmap cons '(1 2 3))
(exn-tester 3 "ormap" intermediate-ormap add1 1)
(exn-tester 4 "andmap" intermediate-andmap add1 1)
(exn-tester 5 "ormap" intermediate-ormap (lambda (x) x) '(1 2 3))
(exn-tester 6 "andmap" intermediate-andmap (lambda (x) x) '(1 2 3))
(exn-tester 7 "andmap" intermediate-andmap add1 '(1 2 3))
(exn-tester 8 "filter" intermediate-filter (lambda (x) x) '(1 2 3))
(exn-tester 9 "filter" intermediate-filter add1 '(1 2 3))
(exn-tester 10 "sort" intermediate-sort '(1 2 3) (lambda (x y) (if (< x y) y #false)))
(exn-tester 11 "quick" intermediate-quicksort '(1 2 3) (lambda (x y) (if (< x y) y #false)))

(unless (equal? (intermediate-ormap odd? '(1 2 3)) #t) (error 'x "1"))
(unless (equal? (intermediate-andmap (lambda (f x) (f x)) (list odd? even? odd?) '(1 2 3)) #t)
  (error 'x "1a"))
(unless (equal? (intermediate-ormap (lambda (f x) (f x)) (list even? odd? odd?) '(1 2 3)) #t)
  (error 'x "1b"))
(unless (equal? (intermediate-andmap odd? '(1 2 3)) #f) (error 'x "2"))
(unless (equal? (intermediate-andmap odd? '(1 3 5)) #t) (error 'x "3"))
(unless (equal? (intermediate-ormap even? '(1 3 5)) #f) (error 'x "4"))
(unless (equal? (intermediate-filter odd? '(1 2 3)) '(1 3)) (error 'x "5"))
(unless (equal? (intermediate-filter odd? '()) '()) (error 'x "6"))
(unless (equal? (intermediate-sort '(1 0 2) <) '(0 1 2)) (error 'x "7"))
(unless (equal? (intermediate-quicksort '(1 0 2) <) '(0 1 2)) (error 'x "8"))
|#
