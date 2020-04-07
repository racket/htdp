#lang typed/racket

;; this file contains the parts of the view-controller
;; that can easily be represented using typed racket.

(provide (struct-out Step)
         application-step?
         finished-stepping-step?
         ;; WIP:
         (struct-out Step-Repository)
         new-step-repository
         add-step!
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

;; WIP

(struct Step-Repository ([view-history : (Listof Step)] [num-steps-available : Natural])
  #:mutable)

(define (new-step-repository) : Step-Repository (Step-Repository '() 0))

(define (add-step! [repo : Step-Repository] [new-step : Step]) : Void
  (set-Step-Repository-view-history! repo
                                     (append (Step-Repository-view-history repo)
                                             (list new-step)))
  (set-Step-Repository-num-steps-available! repo (length (Step-Repository-view-history repo))))




  ;; find-later-step : given a predicate on history-entries, search through
  ;; the history for the first step that satisfies the predicate and whose
  ;; number is greater than n (or -1 if n is #f), return # of step on success,
  ;; on failure return (list 'nomatch last-step) or (list 'nomatch/seen-final
  ;; last-step) if we went past the final step
  #;(: find-later-step ((step -> Boolean) (U Number False) (U Natural
                                                            (List 'nomatch Natural)
                                                            (List 'nomatch/seen-final Natural))))
(define (find-later-step [repo : Step-Repository] [p : (Step -> Boolean)] [n : (U False Integer)])
  (let* ([n-as-num (or n -1)])
    (let loop
      : (U Integer (List 'nomatch Integer) (List 'nomatch/seen-final Integer))
      ([step : Integer 0]
       [remaining (Step-Repository-view-history repo)]
       [seen-final? : Boolean #f])
      (cond [(null? remaining)
             (cond [seen-final? (list `nomatch/seen-final (- step 1))]
                   [else (list `nomatch (- step 1))])]
            [(and (> step n-as-num) (p (car remaining))) step]
            [else (loop (+ step 1)
                        (cdr remaining)
                        (or seen-final? (finished-stepping-step? (car remaining))))]))))

;; find-earlier-step : like find-later-step, but searches backward from
;; the given step.
(define (find-earlier-step [repo : Step-Repository] [p : (Step -> Boolean)] [n : (U False Integer)])
  (unless (number? n)
    (error 'find-earlier-step
           "can't find earlier step when no step is displayed."))
  (let* ([to-search (reverse (take (Step-Repository-view-history repo) n))])
    (let loop
      : (U Integer 'nomatch (List 'nomatch/seen-final Integer))
      ([step (- n 1)]
       [remaining to-search])
      (cond [(null? remaining) `nomatch]
            [(p (car remaining)) step]
            [else (loop (- step 1) (cdr remaining))]))))

(module+ test
  (require typed/rackunit)

  (define repo (new-step-repository))

  ;; there simply is no good answer for this in the current design.
  (check-equal? (find-later-step repo (λ (s) #t) #f)
                'abc))




