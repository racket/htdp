(load "tester.ss")

;; ------------------------------------------------------------------------
;; testing repl
;; teachpack: guess.ss

;; check-guess : number number -> symbol 
;; to determine how guess and target relate to each other 
(define (check-guess guess target)
  (cond
    ((< target guess) 'TooLarge)
    ((= target guess) 'Perfect)
    ((> target guess) 'TooSmall)))

;; Tests: 
(eq? (check-guess 5000 5631) 'TooSmall)
(eq? (check-guess 6000 5631) 'TooLarge)
(eq? (check-guess 5631 5631) 'Perfect)

;; Test with GUI lib: set lib to guess-lib.ss
(repl check-guess)

(test-error (repl list))
(test-error (repl first))

