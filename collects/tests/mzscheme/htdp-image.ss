;; Load this one with MrEd

(load-relative "loadtest.ss")

(define current-htdp-lang '(lib "htdp-beginner.ss" "lang"))
(load-relative "htdp-test.ss")

;; check-on-bitmap : symbol snip -> void
;; checks on various aspects of the bitmap snips to make
;; sure that they draw properly
(define (check-on-bitmap name snp)
  (let-values ([(width height) (send snp get-size)])
    (let ([bdc (make-object bitmap-dc%)])
      
      ;; test that no drawing is outside the snip's drawing claimed drawing area
      (let ([bm-clip (make-object bitmap% (+ width 100) (+ height 100))]
            [bm-noclip (make-object bitmap% (+ width 100) (+ height 100))]
            [s-clip (make-string (* (+ width 100) (+ height 100) 4))]
            [s-noclip (make-string (* (+ width 100) (+ height 100) 4))])
        (send bdc set-bitmap bm-clip)
        (send bdc clear)
        (send bdc set-clipping-rect 50 50 width height)
        (send snp draw bdc 50 50 0 0 (+ width 100) (+ height 100) 0 0 #f)
        (send bdc set-clipping-region #f)
        (send bdc get-argb-pixels 0 0 (+ width 100) (+ height 100) s-clip)
        
        (send bdc set-bitmap bm-noclip)
        (send bdc clear)
        (send snp draw bdc 50 50 0 0 (+ width 100) (+ height 100) 0 0 #f)
        (send bdc get-argb-pixels 0 0 (+ width 100) (+ height 100) s-noclip)
        (send bdc set-bitmap #f)
        (test (list 'bmclip name #t) (lambda () (list 'bmclip name (string=? s-clip s-noclip)))))
      
      (let ([bm-normal (make-object bitmap% width height)]
            [bm-bitmap (make-object bitmap% width height)]
            [s-normal (make-string (* width height 4))]
            [s-bitmap (make-string (* width height 4))])
        
        (send bdc set-bitmap bm-normal)
        (send bdc clear)
        (send snp draw bdc 0 0 0 0 width height 0 0 #f)
        (send bdc get-argb-pixels 0 0 width height s-normal)
        (send bdc set-bitmap bm-bitmap)
        (send bdc clear)
        
        ;; force the snip to switch over to bitmap mode
        (send snp get-bitmap)
        
        (send snp draw bdc 0 0 0 0 width height 0 0 #f)
        (send bdc get-argb-pixels 0 0 width height s-bitmap)
        (send bdc set-bitmap #f)
        (test (list 'bmsame name #t) (lambda () (list 'bmsame name (string=? s-normal s-bitmap))))))))

(require (lib "htdp-beginner.ss" "lang"))
      

(htdp-test #t 'image? (image? (filled-rect 10 10 'blue)))
(htdp-test #f 'image? (image? 5))

(define red (make-color 248 20 64))
(define blue (make-color 80 80 248))
(define black (make-color 0 0 0))
(define white (make-color 255 255 255))
(htdp-top (define red (make-color 248 20 64)))
(htdp-top (define blue (make-color 80 80 248)))
(htdp-top (define black (make-color 0 0 0)))
(htdp-top (define white (make-color 255 255 255)))

(htdp-test (list red)
	   'color-list
	   (image->color-list (filled-rect 1 1 'red)))

(htdp-test (list red blue black white)
	   'colors-set-up-properly
	   (map car (list (image->color-list (filled-rect 1 1 'red))
                          (image->color-list (filled-rect 1 1 'black))
                          (image->color-list (filled-rect 1 1 'blue))
                          (image->color-list (filled-rect 1 1 'white)))))

(htdp-test (list blue blue blue blue)
	   'color-list
	   (image->color-list (filled-rect 2 2 'blue)))

(htdp-test #t
	   'color-list
	   (image=? (color-list->image (list blue blue blue blue) 2 2)
		    (filled-rect 2 2 'blue)))
(htdp-test #f
	   'color-list
	   (image=? (color-list->image (list blue blue blue blue) 2 2)
		    (filled-rect 1 4 'blue)))
(htdp-test #t
	   'color-list
	   (image=? (color-list->image (list blue blue blue blue) 1 4)
		    (filled-rect 1 4 'blue)))

(htdp-test #t
           'mask-color-list1
           (= (image-width
               (mask-color-lists->image
                (list red   white red
                      white white white)
                (list black white black
                      white white white)
                3
                2))
              3))

(htdp-test #t
           'mask-color-list2
           (= (image-height
               (mask-color-lists->image
                (list red   white red
                      white white white)
                (list black white black
                      white white white)
                3
                2))
              2))

(htdp-test #t
           'mask-color-list3
           (equal? (image->color-list
                    (mask-color-lists->image
                     (list red   red   red
                           red   red   red)
                     (list black white black
                           white white white)
                     3 2))
                   (list red   white red
                         white white white)))
(htdp-test #t
           'mask-color-list4
           (equal? (image->color-list
                    (image+
                     (filled-rect 3 3 'blue)
                     (mask-color-lists->image
                      (list red   white red
                            white white white
                            red   white red)
                      (list black white black
                            white white white
                            black white black)
                      3
                      3)))
                   (list red  blue red
                         blue blue blue
                         red  blue red)))

(htdp-test #t
	   'image+
	   (image=? (color-list->image (list blue red blue red) 2 2)
		    (image+ (filled-rect 2 2 'red)
			    (filled-rect 1 2 'blue))))

(htdp-test #t
           'image+/empty-spaces-are-unmasked
           (image=? (color-list->image (list red red red blue) 2 2)
                    (image+
                     (filled-rect 2 2 'blue)
                     (image+ (filled-rect 1 2 'red)
                             (filled-rect 2 1 'red)))))

(htdp-test #t
	   'offset-image+1
	   (image=? (color-list->image (list red blue red blue) 2 2)
		    (offset-image+ (filled-rect 2 2 'red)
				   1 0
				   (filled-rect 1 2 'blue))))

(htdp-test #t
	   'offset-image+2
	   (image=? (color-list->image (list red red red blue) 2 2)
		    (offset-image+ (filled-rect 2 2 'red)
				   1 1
				   (filled-rect 1 1 'blue))))

(htdp-test #t
	   'offset-image+3
	   (image=? (color-list->image (list red red blue blue) 2 2)
		    (offset-image+ (filled-rect 2 1 'red)
				   0 1
				   (filled-rect 2 1 'blue))))

(htdp-test #t
	   'offset-image+4
	   (image=? (color-list->image (list blue blue red red) 2 2)
		    (offset-image+ (filled-rect 2 1 'red)
				   0 -1
				   (filled-rect 2 1 'blue))))

(htdp-test #t
           'offset-image+/white
           (image=? (color-list->image (list black black black
                                             black white black
                                             black black black)
                                       3 3)
                    (offset-image+ (filled-rect 3 3 'black)
                                   1 1
                                   (filled-rect 1 1 'white))))

(htdp-test #t
           'color-list->image/white-in-mask
           (image=? (color-list->image (list black red   black
                                             red   red   red
                                             black red   black)
                                       3 3)
                    (image+ (filled-rect 3 3 'red)
                            (color-list->image (list black white black
                                                     white white white
                                                     black white black)
                                               3 3))))


(htdp-test #t
	   'image+
	   (image=? (color-list->image (list red blue red red blue red) 3 2)
		    (offset-image+ (filled-rect 3 2 'red)
				   1 0
				   (filled-rect 1 2 'blue))))

(htdp-test #t
	   'image-inside?
	   (image-inside? (offset-image+ (filled-rect 3 2 'red)
					 1 0
					 (filled-rect 1 2 'blue))
			  (filled-rect 1 2 'blue)))

(htdp-test #f
	   'image-inside?
	   (image-inside? (offset-image+ (filled-rect 3 2 'red)
					 1 0
					 (filled-rect 1 2 'blue))
			  (filled-rect 1 2 'black)))

(htdp-test #t
	   'image-inside?
	   (image-inside? (offset-image+ (filled-rect 3 2 'red)
					 1 0
					 (filled-rect 1 2 'blue))
			  (filled-rect 1 2 'red)))

(htdp-test #f
	   'image-inside?
	   (image-inside? (offset-image+ (filled-rect 3 2 'red)
					 1 0
					 (filled-rect 1 2 'blue))
			  (filled-rect 2 1 'red)))


(htdp-test #t
	   'image-inside?
	   (image-inside? (offset-image+ (filled-rect 3 2 'red)
					 1 0
					 (filled-rect 1 2 'blue))
			  (color-list->image (list red white white) 
					     3 1)))

(htdp-test #f
	   'image-inside?
	   (image-inside? (offset-image+ (filled-rect 3 2 'red)
					 1 0
					 (filled-rect 1 2 'blue))
			  (color-list->image (list blue white white) 
					     3 1)))

(htdp-test #t
	   'image-inside?
	   (image-inside? (offset-image+ (filled-rect 3 2 'red)
					 1 0
					 (filled-rect 1 2 'blue))
			  (color-list->image (list white blue white) 
					     3 1)))

(htdp-test (make-posn 2 5)
	   'find-image
	   (find-image (offset-image+ (filled-rect 16 16 'red)
				      2 5
				      (outline-circle 6 6 'blue))
		       (outline-circle 6 6 'blue)))

(htdp-test (make-posn 0 0)
	   'find-image
	   (find-image (filled-rect 16 16 'blue)
		       (outline-circle 6 6 'blue)))

(htdp-test 5
	   'image-width
	   (image-width (filled-rect 5 7 'red)))

(htdp-test 7
	   'image-height
	   (image-height (filled-rect 5 7 'red)))

(htdp-test 1 'color-red (color-red (make-color 1 2 3)))
(htdp-test 2 'color-green (color-green (make-color 1 2 3)))
(htdp-test 3 'color-blue (color-blue (make-color 1 2 3)))
(htdp-test #t 'color? (color? (make-color 1 2 3)))
(htdp-test #f 'color? (color? 10))

(htdp-test #t
           'line 
           (image=? (line 4 0 'red)
                    (color-list->image (list red red red red red) 5 1)))

(htdp-test #t
           'line 
           (image=? (line 0 4 'red)
                    (color-list->image (list red red red red red) 1 5)))

;; note: next two tests may be platform-specific... I'm not sure.
;; I developed them under macos x. -robby
(htdp-test #t
           'outline-triangle
           (image=? (outline-triangle 3 'red)
                    (color-list->image 
                     (list white red   white
                           white red   white
                           red   white red
                           red   red   red)
                     3
                     4)))

(htdp-test #t
           'filled-triangle
           (image=? (filled-triangle 3 'red)
                    (color-list->image 
                     (list white red   white
                           white red   white
                           red   red   red
                           red   red   red)
                     3
                     4)))


(check-on-bitmap 'filled-rect (htdp-eval (filled-rect 2 2 'red)))
(check-on-bitmap 'outline-rect (htdp-eval (outline-rect 2 2 'red)))
(check-on-bitmap 'filled-circle (htdp-eval (filled-circle 2 4 'red)))
(check-on-bitmap 'outline-circle (htdp-eval (outline-circle 2 4 'red)))
(check-on-bitmap 'filled-triangle (htdp-eval (filled-triangle 10 'red)))
(check-on-bitmap 'outline-triangle (htdp-eval (outline-triangle 10 'red)))
(check-on-bitmap 'line (htdp-eval (line 10 7 'red)))
(check-on-bitmap 'text (htdp-eval (text "XX" 'red)))
(check-on-bitmap 'anti-alias-text (htdp-eval (anti-alias-text "XX" 'red)))
(check-on-bitmap 'image+ (htdp-eval (image+ (filled-rect 1 4 'blue) (filled-rect 4 1 'green))))
(check-on-bitmap 'image+ (htdp-eval (offset-image+ (filled-rect 4 4 'blue)
                                                   2 2
                                                   (filled-rect 4 4 'green))))
(check-on-bitmap 'mask-color-lists
                 (htdp-eval
                  (image+
                   (filled-rect 3 3 'blue)
                   (mask-color-lists->image
                    (list red   white red
                          white white white
                          red   white red)
                    (list black white black
                          white white white
                          black white black)
                    3
                    3))))


(report-errs)