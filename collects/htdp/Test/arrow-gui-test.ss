;; TeachPack : arrow-gui.ss
;; Language: Advanced 

;; make-model : sym -> (button% event% -> void)
(define (make-model dir)
  (lambda (b e)
    (printf (format "~a" (control)))
    (view dir)))

(connect 
 (make-model "west")
 (make-model "east")
 (make-model "north")
 (make-model "south"))
  