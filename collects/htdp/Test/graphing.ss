;; TeachPack: graphing.ss
;; Language: Intermediate

;; ------------------------------------------------------------------------

(define (f x) 
  (+ (* 1/60 (* x x x))
     (* -1/10 (* x x))
     5))

(define (fp x)
  (+ (* 5/100 (* x x))
     (* -1/5 x)))

;y - y1    y2 - y1
;------- = -------
;x - x1    x2 - x1

(define (posns-line p1 p2)
  (local ((define delta-x (- (posn-x p1) (posn-x p2)))
	  (define delta-y (- (posn-y p1) (posn-y p2)))
	  (define slope (/ delta-y delta-x)))
    (lambda (x)
      (+ (* slope (- x (posn-x p1))) (posn-y p1)))))
	 

; (graph-fun f RED)
; (graph-line (posns-line (make-posn 2 (f 2)) (make-posn 6 (f 6))) BLACK)
; (graph-line (posns-line (make-posn 3 (f 3)) (make-posn 5 (f 5))) BLUE)
; (graph-line (posns-line (make-posn 3.5 (f 3.5)) (make-posn 4.5 (f 4.5))) GREEN)
; (graph-line (posns-line (make-posn 3.9 (f 3.9)) (make-posn 4.1 (f 4.1))) RED)
(graph-fun (lambda (x) (+ (* +1 x) 4)) RED)
(graph-fun (lambda (x) (+ (* -1 x) 4)) BLUE)
(graph-line (lambda (x) (+ (* +1 x) 10)) BLACK)
(graph-line (lambda (x) (+ (* -1 x) 10)) GREEN)

;; \scheme{d/dx : (num -> num) -> (num -> num)}
(define (d/dx f)
  (local ((define (fprime x)
	    (/ (- (f (+ x $\eps$)) (f (- x $\eps$)))
	       (* 2 $\eps$)))
	  (define $\eps$ .0001))
    fprime))

(define f (lambda (x) (+ (* (- x 3) (- x 1)) 4)))
