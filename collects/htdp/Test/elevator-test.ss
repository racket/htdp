(define (next3 x y z)
  (cond
    ((and (eq? 'up x) (< y 8)) (+ y 1))
    ((eq? 'up x) 7) ; anything down
    ((and (eq? 'down x) (> y 1)) (- y 1))
    (else 2))) ; anything up

; (run next3)