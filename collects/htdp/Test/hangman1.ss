(load "tester.ss")
;; by hand, Beginner for plain, Full for errors 

(define-struct word (a b c))
;; A word is a structure: (make-word letter letter letter)

;; DEFINITION: 
(define (reveal chosen status guess)
  (make-word (reveal1 (word-a chosen) (word-a status) guess)
             (reveal1 (word-b chosen) (word-b status) guess)
             (reveal1 (word-c chosen) (word-c status) guess)))

;; EXAMPLES:
;; reveal1: 'a 'a 'x => 'a 
;; reveal1: 'x '_ 'x => 'x
;; reveal1: 'x '_ 'y => '_

;; The inputs are atomic, which means we need domain knowledge.
;; The domain knowledge is given in the problem statement. 

;; DEFINITION: 
(define (reveal1 ch st gu) 
  (cond
    ((eq? ch st) ch)
    ((eq? ch gu) gu)
    (else st)))

; -------------------------------------------------------------------------

;; draw-next-part : symbol -> true
;; consumes one of the seven body-part symbols and draws that part.

(define (draw-next-part body-part)
  (cond 
    [(eq? body-part 'body)
     (draw-solid-line (make-posn 100 60)
                      (make-posn 100 130)
                      BLACK)]
    [(eq? body-part 'right-leg)
     (draw-solid-line (make-posn 100 130)
                      (make-posn 30 170)
                      BLACK)]
    [(eq? body-part 'left-leg)
     (draw-solid-line (make-posn 100 130)
                      (make-posn 170 170)
                      BLACK)]
    [(eq? body-part 'right-arm)
     (draw-solid-line (make-posn 100 75)
                      (make-posn 40 65)
                      BLACK)]
    [(eq? body-part 'left-arm)
     (draw-solid-line (make-posn 100 75)
                      (make-posn 160 65)
                      BLACK)]
    [(eq? body-part 'head)
     (draw-solid-disk (make-posn 100 50) 10 BLACK)]
    [(eq? body-part 'noose)
     (and
      (draw-solid-disk (make-posn 120 50) 30 RED)
      (draw-solid-line (make-posn 100 30)
                       (make-posn 100 10)
                       BLACK)
      (draw-solid-line (make-posn 100 10)
                       (make-posn 0 10)
                       BLACK)
      (draw-solid-line (make-posn 115 35)
                       (make-posn 123 43)
                       BLACK)
      (draw-solid-line (make-posn 123 35)
                       (make-posn 115 43)
                       BLACK)
      (draw-solid-line (make-posn 131 40)
                       (make-posn 139 48)
                       BLACK)
      (draw-solid-line (make-posn 139 40)
                       (make-posn 131 48)
                       BLACK))]))

#| -------------------------------------------------------------------------
   TESTS:
|#
  
(eq? (reveal1 'a 'a 'x) 'a)
(eq? (reveal1 'x '_ 'x) 'x)
(eq? (reveal1 'x '_ 'y) '_)

(equal? (reveal (make-word 'd 'e 'r) (make-word '_ '_ '_) 'd)
        (make-word 'd'_ '_))
(equal? (reveal (make-word 'd 'e 'r) (make-word '_ '_ '_) 'f)
        (make-word '_ '_ '_))

(start 200 400)
; (hangman make-word reveal draw-next-part)

(test-error (hangman make-word))
(test-error (hangman (make-word 'a 'b 'c) reveal draw-next-part))
(test-error 
 (hangman make-word
          (reveal (make-word 'd 'e 'r) (make-word '_ '_ '_) 'd)
          draw-next-part))
(test-error (hangman make-word reveal 100))
