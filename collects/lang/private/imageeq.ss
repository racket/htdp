(module imageeq mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (define util-dc #f)

  (define (image? a)
    (is-a? a image-snip%))

  (define (image=? a b)
    (unless (is-a? a image-snip%)
      (raise-type-error 'image=? "image" 0 a b))
    (unless (is-a? b image-snip%)
      (raise-type-error 'image=? "image" 1 a b))

    (unless util-dc
      (set! util-dc (make-object bitmap-dc% (make-object bitmap% 2 2))))

    ;; First, get sizes:
    (let ([dc util-dc]
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
	     (let* ([w (ub a-wb)]
		    [h (ub a-hb)]
		    [s1 (make-string (* w h 4))]
		    [s2 (make-string (* w h 4))])
	       (send a-dc get-argb-pixels 0 0 w h s1)
	       (send b-dc get-argb-pixels 0 0 w h s2)
	       (string=? s1 s2))))))
  
  (provide image? image=?))

      
