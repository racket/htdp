(module imageeq mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (define (image=? a b)
    (unless (a . is-a? . image-snip%)
      (raise-type-error 'image=? "image" 0 a b))
    (unless (b . is-a? . image-snip%)
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
	   (let ([a-dc (make-object bitmap-dc% (make-object bitmap% (unbox a-wb) (unbox a-hb)))]
		 [b-dc (make-object bitmap-dc% (make-object bitmap% (unbox b-wb) (unbox b-hb)))])
	     (send a draw a-dc 0 0 0 0 (unbox a-wb) (unbox a-hb) 0 0 #f)
	     (send b draw b-dc 0 0 0 0 (unbox a-wb) (unbox a-hb) 0 0 #f)

	     ;; Compare pixels:
	     #t))))

  (provide image=?))

      
