;; TeachPack: gui.ss
;; Language Level: Advanced 

;; type in text, choose, click okay, watch message field change, close

(define msg (make-message "Hello World"))

(define txt (make-text "Enter your password please"))

(define chc (make-choice (list "Choose something" "Or other")))

(create-window
 (list (list txt msg chc)
       (list (make-button "Okay?" 
                          (lambda (x)
                            (begin
                              (printf " ~s ~s~n"
                                      (choice-index chc) 
                                      (text-contents txt))
                              (draw-message msg "Bye World")))))
       (list (make-button "Close?" hide-window))))

