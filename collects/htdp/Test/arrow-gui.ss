;; TeachPack : arrow-gui.ss
;; Language: Advanced 

;; make-model : sym -> (button% event% -> void)
(define (make-model dir)
  (lambda (b e)
    (begin
      (printf "~a ~n" (control))
      (view dir))))

(connect 
 (make-model "left")
 (make-model "right")
 (make-model "up")
 (make-model "down"))
  