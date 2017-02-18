#lang racket
(require graphics/value-turtles rackunit)
(check-equal? (turtle-state (turtles 10 10))
              (list (vector-immutable 5 5 0)))
(check-equal? (turtle-state (turn/radians 2 (turtles 10 10)))
              (list (vector-immutable 5 5 -2)))
(check-equal? (turtle-state (merge (turtles 10 10)
                                   (turn/radians -2 (turtles 10 10))))
              (list (vector-immutable 5 5 2)
                    (vector-immutable 5 5 0)))
(check-equal? (turtle-state (clean (turtles 10 10)))
              (list))
(check-equal? (turtle-state (merge (clean (turtles 10 10))
                                   (turtles 10 10)))
              (list (vector-immutable 5 5 0)))
(check-equal?
 (let* ([t1 (turtles 10 10)]
        [t2 (merge t1 (move 1 t1))]
        [t3 (merge t2 (move 3 t2))])
   (turtle-state t3))
 '(#(9 5 0) #(8 5 0) #(6 5 0) #(5 5 0)))

(check-equal? (turtles-width (turtles 10 20)) 10)
(check-equal? (turtles-height (turtles 10 20)) 20)

(define (get/set-turtle-state tv)
  (define s (turtle-state tv))
  (same? (turtle-state
          (restore-turtle-state (turtles (turtles-width tv)
                                         (turtles-height tv))
                                s))
         s))

(define (same? a b)
  (cond
    [(and (list? a) (list? b) (= (length a) (length b)))
     (for/and ([a (in-list a)]
               [b (in-list b)])
       (same? a b))]
    [(and (real? a) (real? b)) (= a b)]
    [(and (vector? a) (vector? b) (= (vector-length a) (vector-length b)))
     (for/and ([a (in-vector a)]
               [b (in-vector b)])
       (same? a b))]
    [else (equal? a b)]))

(check-true (get/set-turtle-state (turtles 100 200)))
(check-true (get/set-turtle-state (clean (turtles 100 200))))
(check-true (get/set-turtle-state (move 30 (turtles 100 200))))
(check-true (get/set-turtle-state (move -30 (turtles 100 200))))
(check-true (get/set-turtle-state (move -30 (turn 90 (turtles 100 200)))))
(check-true (get/set-turtle-state (move -30 (turn 120 (turtles 100 200)))))
(check-true (get/set-turtle-state (merge (turtles 100 200)
                                         (move -30 (turn 120 (turtles 100 200))))))
(check-true (get/set-turtle-state (merge (move -30 (turn 120 (turtles 100 200)))
                                         (turtles 100 200))))
