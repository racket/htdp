(module master-play mzscheme
  (require "master.ss")
  (provide go)
  
  (define (go s)
    (printf "Have fun playing, ~a~n" s)
    (master 
     (lambda (choice1 choice2 guess1 guess2)
       (cond
         [(and (eq? choice1 guess1) (eq? choice2 guess2))
          'perfect!]
         [(or (eq? choice1 guess1) (eq? choice2 guess2))
          'one_color_is_at_proper_place]
         [(or (eq? choice2 guess1) (eq? choice1 guess2))
          'one_color_occurs]
         [else
          'sorry_all_wrong])))))