;; TeachPack : arrow-gui.ss
;; Language: Advanced 

;; make-model : sym -> (button% event% -> void)
(define (make-model dir)
  (lambda (b e)
    (local ([define _ (view dir)])
      (printf "~a ~n" (control)))))

(connect 
 (make-model "left")
 (make-model "right")
 (make-model "up")
 (make-model "down"))
  