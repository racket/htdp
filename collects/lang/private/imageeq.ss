(module imageeq mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (define (image=? a b)
    (unless (is-a? a image-snip%)
      (raise-type-error 'image=? "image" 0 a b))
    (unless (is-a? b image-snip%)
      (raise-type-error 'image=? "image" 1 a b))

    ;; First, get sizes:
    (let ([dc (make-object bitmap-dc% (make-object bitmap% 2 2))]
	  [a-wb (box 0)] [a-hb (box 0)]
	  [b-wb (box 0)] [b-hb (box 0)])
      (send a get-extent dc 0 0 a-wb a-hb #f #f #f #f)
      (send b get-extent dc 0 0 b-wb b-hb #f #f #f #f)

      ;; If the same size...
      (and (= (unbox a-wb) (unbox b-wb))
	   (= (unbox a-hb) (unbox b-hb))

	   ;; Get a & b bitmaps:
	   (let* ([ub (lambda (b) (inexact->exact (ceiling (unbox b))))]
		  [a-dc (make-object bitmap-dc% (make-object bitmap% (ub a-wb) (ub a-hb)))]
		  [b-dc (make-object bitmap-dc% (make-object bitmap% (ub b-wb) (ub b-hb)))])
	     (send a draw a-dc 0 0 0 0 (unbox a-wb) (unbox a-hb) 0 0 'no-caret)
	     (send b draw b-dc 0 0 0 0 (unbox a-wb) (unbox a-hb) 0 0 'no-caret)

	     ;; Compare pixels:
	     (let ([c1 (make-object color%)]
		   [c2 (make-object color%)]
		   [w (ub a-wb)]
		   [h (ub a-hb)])
	       (with-method ([get-pixel-a (a-dc get-pixel)]
			     [get-pixel-b (b-dc get-pixel)]
			     [red1 (c1 red)]
			     [green1 (c1 green)]
			     [blue1 (c1 blue)]
			     [red2 (c2 red)]
			     [green2 (c2 green)]
			     [blue2 (c2 blue)])
		 (let iloop ([i 0])
		   (if (= i w)
		       #t
		       (let jloop ([j 0])
			 (if (= j h)
			     (iloop (add1 i))
			     (begin
			       (get-pixel-a i j c1)
			       (get-pixel-b i j c2)
			       (and (= (red1) (red2))
				    (= (green1) (green2))
				    (= (blue1) (blue2))
				    (jloop (add1 j))))))))))))))
  
  (provide image=?))

      
