(module guess mzscheme
  (require (lib "error.ss" "htdp")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred"))

  (provide guess-with-gui guess-with-gui-3 guess-with-gui-list)
  
      #| ------------------------------------------------------------------------
      The Basic Constants |#
      
      (define TITLE "Bobby's Game")
      (define WELCOME "Welcome to Bobby's Game") 
      
      (define GUESS 5)
      
      (define DIGITS (build-list 10 (lambda (i) (number->string i))))
      
      (define BUT-SIZE 10)
      (define WIDTH (* GUESS BUT-SIZE))
      (define HIGHT BUT-SIZE)
      
      (define STOPS (list 'Perfect 'perfect 'perfect! 'perfect_guess))
      
      (define TRUMPET
        (make-object bitmap% 
          (build-path (collection-path "icons") "trumpet.xbm")
          'xbm))
      
    ;; ------------------------------------------------------------------------
    ;; State of Game 
      
    ;; the-number : randomly chosen 
      (define the-number 0)
      (define (make-target)
        (set! the-number (random (expt 10 GUESS))))
      
      #| ------------------------------------------------------------------------
      The Layout: (computed as a function of constants)
      
      ------------------------------------------------------------------
      |       
      |   CB1  CB2   CB3   CB4   .......   CB*   CB*   CB*   CB*   CB* 
      | 
      |          CB-GUESS1    ...    CB-GUESS*
      | 
      |                     ONE-MESSAGE
      ------------------------------------------------------------------
      
      Two horizontal panels: 
      the first one with all the colors (as buttons)
      the second is a sequence of colored buttons 
      |#
      (define frame (make-object frame% TITLE #f WIDTH HIGHT))
      (define verti (make-object vertical-panel% frame))
      (define panel (make-object horizontal-panel% verti))
      (send panel set-alignment 'center 'center)
      
      (define guess-panel (make-object horizontal-panel% verti))
      (send guess-panel set-alignment 'center 'center)
      
      (define message-panel (make-object horizontal-panel% verti))
      (send message-panel set-alignment 'center 'center)
      
    ;; message : a field for displaying basic messages about state of game 
      (define message #f)
      (define (add-message!)
        (send message-panel change-children (lambda (x) null))
        (set! message (make-object message% WELCOME message-panel)))
      
    ;; new-game : (args ANY) -> void
    ;; effect: set up new target number, send message that game's ready 
      (define (new-game . x) 
        (make-target)
        (add-message!))
      
    ;; ------------------------------------------------------------------------
    ;; Setting up the buttons
      
      (define game-going #f)
      (define (init-game number-of-digits)
        (if game-going
            (error 'repl "The game can be started only once.")
            (set! game-going #t))
        (set! GUESS number-of-digits)
        (new-game)
        (local (;; make-choice : str -> button%
              ;; create a choice for DIGITS
              ;; and a call-back function that
              ;;   -- sets the n-th digit to choice
                (define (make-choice n)
                  (local ((define (this x y)
                            (vector-set! guesses n (send x get-selection))))
                    (make-object choice% #f DIGITS panel this)))
              ;; effect only: 
                (define _setup_choices 
                  (for-each (lambda (i) 
                              (make-choice (- GUESS i 1))) 
                            (build-list GUESS identity)))            
              ;; add-winner! : -> void
              ;; effect: announce winner and set up new game
                (define (add-winner!)
                  (send message-panel change-children (lambda (x) empty))
                  (make-object message% TRUMPET message-panel)
                  (make-object button% "New Game?" message-panel new-game)
                  (make-object button% "CLOSE?" message-panel
                    (lambda (x y)
                      (send frame show #f))))
              ;; guesses: status vector
                (define guesses (make-vector GUESS 0))
              ;; call-back : _ _ -> void
              ;; check status and announce result, possibly set winner
                (define (call-back x y)
                  (let ((response (check-guess (convert guesses) the-number)))
                    (send message set-label (symbol->string response))
                    (when (memq response STOPS)
                      (add-winner!)))))
          (make-object button% "Check" guess-panel call-back)))
      
    ;;  ------------------------------------------------------------------------
    ;; Interacting with the Student Contribution 
      
    ;; convert : (vector DIGIT) -> number
    ;; to convert a vector of digits into a number 
    ;; 0-th digit is right-most digit in number, 
    ;; N-th digit is left-most digit in number
      (define (convert guesses:vec)
        (local ((define (convert digits)
                  (cond
                    ((empty? digits) 0)
                    (else (+ (first digits) (* (convert (rest digits)) 10))))))
          (convert (vector->list guesses:vec))))
      
    ;; guess-with-gui : (num num -> num) -> void
    ;; effect: init target, init frame, set the check-guess function and show the frame
      (define (guess-with-gui cg)
        (check-proc 'guess-with-gui cg 2 'first "two arguments")
        (init-game 5)
        (set! check-guess cg)
        (send frame show #t)
        #t)
      
    ;; guess-with-gui-3 : (digit digit digit num -> num) -> void
    ;; effect: init target, init frame, set the check-guess function and show the frame
      (define (guess-with-gui-3 cg)
        (check-proc 'guess-with-gui-3 cg (+ 3 1) 'first "four arguments")
        (init-game 3)
        (set! convert vector->list)
        (set! check-guess (lambda (lod target) (apply cg (append lod (list target)))))
        (send frame show #t)
        #t)
      
    ;; guess-with-gui-list : num ((listof digit) num -> num) -> void
    ;; effect: init target, init frame, set the check-guess function and show the frame
      (define (guess-with-gui-list n cg)
        (check-arg  'guess-with-gui-list
                    (and (number? n) (integer? n) (>= n 1)) "positive integer" '1st n)
        (check-proc 'guess-with-gui-list cg 2 'first "two arguments")
        (init-game n)
        (set! check-guess cg)
        (set! convert vector->list)
        (send frame show #t)
        #t)
      
      (define check-guess #f))
