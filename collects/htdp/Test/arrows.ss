; (load "tester.ss")

;; TeachPack : arrow.ss
;; Language: Beginner

;; ---------------------------------------------------------------------
;; 

;; RAD : the radius of the simple disk moving across a canvas
(define RAD 10)

;; translate : posn number -> posn 
(define (translate sh delta)
  (make-posn (+ (posn-x sh) delta) (posn-y sh)))

;; draw-it : posn -> true
(define (draw-it sh)
  (draw-solid-disk sh RAD))

;; move : posn number -> posn or false 
(define (move sh delta)
  (cond
    [(and (clear-solid-disk sh RAD)
          (draw-solid-disk (translate sh delta) RAD))
     (translate sh delta)]
    [else false]))

;; TESTS: 

(start 100 50)
(control-left-right (make-posn 10 20) 10 move draw-it)

(test-error (control-left-right 'aa 10 move))

;; cannot raise an exception:
;; (control-left-right 'aa 10 move draw-it))
;; shape is polymorphic!!

(test-error (control-left-right 'aa 'bb move draw-it))
(test-error (control-left-right 'aa 10 'cc draw-it))
(test-error (control-left-right 'aa 10 move 'cc))
(test-error (control-left-right 'aa 10 move 'cc))




