#lang typed/racket

;; this file contains the parts of the view-controller
;; that can easily be represented using typed racket.

(provide (struct-out Step)
         application-step?
         finished-stepping-step?
         Step-Repository
         new-step-repository
         make-add-step!
         make-num-steps
         make-step-ref
         find-first-step
         find-later-step
         find-earlier-step
         )

;; the stored representation of a step
(struct Step ([text : Any] ;; should be text%
              [kind : (U 'finished-or-error
                         'user-application
                         'normal)]
              [posns : (Listof Any)]) #:transparent)


;; is this an application step?
(define (application-step? [history-entry : Step]) : Boolean
  (match history-entry
    [(Step _ (or 'user-application 'finished-or-error) posns) #t]
    [else #f]))

;; is this the finished-stepping step?
(define (finished-stepping-step? [history-entry : Step]) : Boolean
  (match (Step-kind history-entry)
    ['finished-or-error #t]
    [else #f]))

;; steps come in one at a time. Steps cannot be removed.
;; all calls are made in the same thread, so I believe
;; race conditions (double add, e.g.) cannot occur.

(define-type Step-Repository (Mutable-HashTable Natural Step))

(define (new-step-repository) : Step-Repository (make-hash))

(define ((make-add-step! [repo : Step-Repository]) [new-step : Step]) : Void
  ;; assuming single-threading, not worrying about race conditions here:
  (hash-set! repo (hash-count repo) new-step))

(define ((make-num-steps [repo : Step-Repository])) : Natural
  (hash-count repo))

(define ((make-step-ref [repo : Step-Repository]) [n : Natural]) : Step
  (hash-ref repo n
            (λ () (error 'step-ref "repo contains no step numbered ~v" n))))




;; find-later-step : given a predicate on history-entries and the # of the
;; current step, search through
;; the history for the first step that satisfies the predicate and whose
;; number is greater than n, return # of step on success,
;; on failure return 'nomatch if there are no matches,
(: find-later-step (All (S) (-> (S -> Boolean) Natural
                                Natural (Natural -> S)
                                (U Natural 'nomatch))))
(define (find-later-step pred? n num-steps step-ref)
  (loop-forward/helper pred? (add1 n) num-steps step-ref))

;; find the first step that satisfies the predicate.
(: find-first-step (All (S) (-> (S -> Boolean)
                                Natural (Natural -> S)
                                (U Natural 'nomatch))))
(define (find-first-step pred? num-steps step-ref)
  (loop-forward/helper pred? 0 num-steps step-ref))

(: loop-forward/helper (All (S) (-> (S -> Boolean) Natural
                                    Natural (Natural -> S)
                                    (U Natural 'nomatch))))
(define (loop-forward/helper pred? n num-steps step-ref)
  (let loop ([i : Natural n])
    (cond [(<= num-steps i) 'nomatch]
          [else (cond [(pred? (step-ref i)) i]
                      [else (loop (add1 i))])])))



(: find-earlier-step (All (S) (-> (S -> Boolean)
                                Natural (Natural -> S)
                                (U Natural 'nomatch))))
(define (find-earlier-step pred? n step-ref)
  (let loop ([i : Integer (sub1 n)])
    (cond [(< i 0) 'nomatch]
          [else (cond [(pred? (step-ref i)) i]
                      [else (loop (sub1 i))])])))



(module+ test
  (require typed/rackunit)

  (define repo (new-step-repository))
  (define step-ref (make-step-ref repo))
  (define num-steps (make-num-steps repo))
  (define add-step! (make-add-step! repo))

  (check-equal? (num-steps) 0)
  (check-exn #px"no step numbered 0"
             (λ () (step-ref 0)))

  (add-step! (Step 'abc 'normal '(abc)))

  (check-equal? (num-steps) 1)
  (check-equal? (step-ref 0) (Step 'abc 'normal '(abc)))

  (add-step! (Step 'def 'normal '(abc)))

  (check-equal? (num-steps) 2)
  (check-equal? (step-ref 0) (Step 'abc 'normal '(abc)))
  (check-equal? (step-ref 1) (Step 'def 'normal '(abc)))
  (check-exn #px"no step numbered 2" (λ () (step-ref 2)))


  (define l '(4 2 -3 4 2 1 2 2 2 1 -3 4 3 4))
  (define (l-ref [i : Integer]) (list-ref l i))

  (check-equal? (find-later-step (λ ([x : Integer]) (< x 0)) 2 (length l) l-ref)
                10)
  (check-equal? (find-later-step (λ ([x : Integer]) (< x 0)) 1 (length l) l-ref)
                2)
  (check-equal? (find-later-step (λ ([x : Integer]) (< x 0)) 10 (length l) l-ref)
                'nomatch)
  (check-equal? (find-later-step (λ ([x : Integer]) (< x 0)) 100 (length l) l-ref)
                'nomatch)
  (check-equal? (find-first-step (λ ([x : Integer]) (< x 0)) (length l) l-ref)
                2)
  (check-equal? (find-first-step (λ ([x : Integer]) (= x 10000)) (length l) l-ref)
                'nomatch)

  (check-equal? (find-earlier-step (λ ([x : Integer]) (< x 0)) 2 l-ref)
                'nomatch)
  (check-equal? (find-earlier-step (λ ([x : Integer]) (< x 0)) 3 l-ref)
                2)
  )




