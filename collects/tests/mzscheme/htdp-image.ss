;; Load this one with MrEd

(load-relative "loadtest.ss")

(define current-htdp-lang '(lib "htdp-beginner.ss" "lang"))
(load-relative "htdp-test.ss")

(require (lib "htdp-beginner.ss" "lang"))

(htdp-test #t 'image? (image? (filled-rect 10 10 'blue)))
(htdp-test #f 'image? (image? 5))

(htdp-test (list (make-color 248 20 64))
	   'color-list
	   (image->color-list (filled-rect 1 1 'red)))

(define red (make-color 248 20 64))
(define blue (make-color 80 80 248))
(define black (make-color 0 0 0))
(define white (make-color 255 255 255))
(htdp-top (define red (make-color 248 20 64)))
(htdp-top (define blue (make-color 80 80 248)))
(htdp-top (define black (make-color 0 0 0)))
(htdp-top (define white (make-color 255 255 255)))

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
	   'image+
	   (image=? (color-list->image (list blue red blue red) 2 2)
		    (image+ (filled-rect 2 2 'red)
			    (filled-rect 1 2 'blue))))

(htdp-test #t
	   'offset-image+
	   (image=? (color-list->image (list red blue red blue) 2 2)
		    (offset-image+ (filled-rect 2 2 'red)
				   1 0
				   (filled-rect 1 2 'blue))))

(htdp-test #t
	   'offset-image+
	   (image=? (color-list->image (list red red red blue) 2 2)
		    (offset-image+ (filled-rect 2 2 'red)
				   1 1
				   (filled-rect 1 1 'blue))))

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