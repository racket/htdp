;; Load this one with MrEd

(load-relative "loadtest.ss")

(require (lib "errortrace.ss" "errortrace"))

(define-values (image-snip1 image-snip2)
  (let ()
    (define size 2)
    
    (define (do-draw c-bm m-bm)
      (let ([bdc (make-object bitmap-dc% c-bm)])
        (send bdc clear)
        (send bdc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
        (send bdc set-brush (send the-brush-list find-or-create-brush "red" 'solid))
        (send bdc draw-rectangle 0 0 size size)
        (send bdc set-bitmap m-bm)
        (send bdc clear)
        (send bdc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
        (send bdc set-brush (send the-brush-list find-or-create-brush "black" 'solid))
        (send bdc draw-rectangle 0 0 (/ size 2) size)
        (send bdc set-bitmap #f)))
    
    (define image-snip1 
      (let* ([c-bm (make-object bitmap% size size)]
             [m-bm (make-object bitmap% size size #t)])
        (do-draw c-bm m-bm)
        (make-object image-snip% c-bm m-bm)))
    
    (define image-snip2 
      (let* ([c-bm (make-object bitmap% size size)]
             [m-bm (make-object bitmap% size size)])
        (do-draw c-bm m-bm)
        (send c-bm set-loaded-mask m-bm)
        (make-object image-snip% c-bm)))

    (values image-snip1 image-snip2)))

;; check-on-bitmap : symbol snip -> void
;; checks on various aspects of the bitmap snips to make
;; sure that they draw properly
(define (check-on-bitmap name snp)
  (let-values ([(width height) (send snp get-size)])
    (let ([bdc (make-object bitmap-dc%)]
          [max-difference
           (lambda (s1 s2)
             (apply max
                    (map (lambda (x y) (abs (- x y))) 
                         (map char->integer (string->list s1))
                         (map char->integer (string->list s1)))))])
      
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
        (send snp get-argb)
        
        (send snp draw bdc 0 0 0 0 width height 0 0 #f)
        (send bdc get-argb-pixels 0 0 width height s-bitmap)
        (send bdc set-bitmap #f)
        (test (list 'bmsame name #t) 
              (lambda () (list 'bmsame name 
                               (<= (max-difference s-normal s-bitmap) 2))))))))

(define current-htdp-lang '(lib "htdp-beginner.ss" "lang"))
(load-relative "htdp-test.ss")
(require (lib "htdp-beginner.ss" "lang"))

(htdp-test #t 'image? (image? (rectangle 10 10 'solid 'blue)))
(htdp-test #f 'image? (image? 5))

(define red (make-color 255 0 0))
(define blue (make-color 0 0 255))
(define black (make-color 0 0 0))
(define white (make-color 255 255 255))

(define awhite (make-alpha-color 0 255 255 255))
(define ablack (make-alpha-color 0 0 0 0))
(define ared (make-alpha-color 0 255 0 0))
(define aclr (make-alpha-color 255 0 0 0))

(htdp-top (define red (make-color 255 0 0)))
(htdp-top (define blue (make-color 0 0 255)))
(htdp-top (define black (make-color 0 0 0)))
(htdp-top (define white (make-color 255 255 255)))

(htdp-top (define awhite (make-alpha-color 0 255 255 255)))
(htdp-top (define ablack (make-alpha-color 0 0 0 0)))
(htdp-top (define ared (make-alpha-color 0 255 0 0)))
(htdp-top (define aclr (make-alpha-color 255 0 0 0)))

(htdp-top (define (p00 i) (move-pinhole i (- (pinhole-x i)) (- (pinhole-y i)))))

(eval `(htdp-top (define image-snip1 (p00 ,image-snip1))))
(eval `(htdp-top (define image-snip2 (p00 ,image-snip2))))

(htdp-test 3 
           'pinhole-x
           (pinhole-x (rectangle 6 8 'solid 'black)))
(htdp-test 4
           'pinhole-y
           (pinhole-y (rectangle 6 8 'solid 'black)))
(htdp-test 1
           'move-pinhole1
           (pinhole-x (move-pinhole (rectangle 6 8 'solid 'black) -2 -2)))
(htdp-test 2
           'move-pinhole2
           (pinhole-y (move-pinhole (rectangle 6 8 'solid 'black) -2 -2)))

(htdp-test (list red)
	   'color-list
	   (image->color-list (rectangle 1 1 'solid 'red)))

(htdp-test (list (list red) (list blue) (list black) (list white))
	   'colors-set-up-properly
	   (list (image->color-list (rectangle 1 1 'solid 'red))
                 (image->color-list (rectangle 1 1 'solid 'blue))
                 (image->color-list (rectangle 1 1 'solid 'black))
                 (image->color-list (rectangle 1 1 'solid 'white))))

(htdp-test (list blue blue blue blue)
	   'color-list
	   (image->color-list (rectangle 2 2 'solid 'blue)))

(htdp-test #t
	   'color-list
	   (image=? (color-list->image (list blue blue blue blue) 2 2 0 0)
		    (rectangle 2 2 'solid 'blue)))
(htdp-test #f
	   'color-list
	   (image=? (color-list->image (list blue blue blue blue) 2 2 0 0)
		    (rectangle 1 4 'solid 'blue)))
(htdp-test #t
	   'color-list
	   (image=? (color-list->image (list blue blue blue blue) 1 4 0 0)
		    (rectangle 1 4 'solid 'blue)))

(htdp-test #t
	   'alpha-color-list1
	   (equal? (make-alpha-color 0 255 0 0)
                   (car (image->alpha-color-list (rectangle 1 1 'solid 'red)))))
(htdp-test #t
	   'alpha-color-list2
	   (equal? (make-alpha-color 0 0 255 0)
                   (car (image->alpha-color-list (rectangle 1 1 'solid 'green)))))
(htdp-test #t
	   'alpha-color-list3
	   (equal? (make-alpha-color 0 0 0 255)
                   (car (image->alpha-color-list (rectangle 1 1 'solid 'blue)))))

(htdp-test #t
           'alpha-color-list4
           (= (image-width
               (alpha-color-list->image
                (list ared aclr ared
                      aclr aclr aclr)
                3
                2
                0
                0))
              3))

(htdp-test #t
           'alpha-color-list5
           (= (image-height
               (alpha-color-list->image
                (list ared aclr ared
                      aclr aclr aclr)
                3
                2
                0
                0))
              2))

(htdp-test #t
           'alpha-color-list6
           (equal? (image->color-list
                    (alpha-color-list->image
                     (list ared aclr ared
                           aclr aclr aclr)
                     3 2 0 0))
                   (list red   white red
                         white white white)))
(htdp-test #t
           'alpha-color-list7
           (equal? (image->color-list
                    (overlay
                     (p00 (rectangle 3 3 'solid 'blue))
                     (p00 (alpha-color-list->image
                           (list ared aclr ared
                                 aclr aclr aclr
                                 ared aclr ared)
                           3
                           3
                           0
                           0))))
                   (list red  blue red
                         blue blue blue
                         red  blue red)))

(htdp-test #t
           'image=?1
           (image=? (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1 0 0)
                    (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1 0 0)))

(htdp-test #t
           'image=?2
           (image=? (alpha-color-list->image (list (make-alpha-color 255 100 100 100)) 1 1 0 0)
                    (alpha-color-list->image (list (make-alpha-color 255 200 200 200)) 1 1 0 0)))

(htdp-test #f
           'image=?3
           (image=? (alpha-color-list->image (list (make-alpha-color 200 100 100 100)) 1 1 0 0)
                    (alpha-color-list->image (list (make-alpha-color 200 200 200 200)) 1 1 0 0)))

(htdp-test #f
           'image=?4
           (image=? (alpha-color-list->image (list (make-alpha-color 200 100 150 175)
                                                   (make-alpha-color 200 100 150 175))
                                             1
                                             2
                                             0
                                             0)
                    (alpha-color-list->image (list (make-alpha-color 200 100 150 175)
                                                   (make-alpha-color 200 100 150 175)) 
                                             2
                                             1
                                             0
                                             0)))

(htdp-test #t
	   'overlay
	   (image=? (color-list->image (list blue red blue red) 2 2 0 0)
		    (overlay (p00 (rectangle 2 2 'solid 'red))
                             (p00 (rectangle 1 2 'solid 'blue)))))

(htdp-test #t
           'overlay/empty-spaces-are-unmasked
           (image=? (color-list->image (list red red red blue) 2 2 0 0)
                    (overlay
                     (p00 (rectangle 2 2 'solid 'blue))
                     (overlay (p00 (rectangle 1 2 'solid 'red))
                              (p00 (rectangle 2 1 'solid 'red))))))

(htdp-test #t
	   'overlay/xy1
	   (image=? (color-list->image (list red blue red blue) 2 2 0 0)
		    (overlay/xy (p00 (rectangle 2 2 'solid 'red))
                                1 0
                                (p00 (rectangle 1 2 'solid 'blue)))))

(htdp-test #t
	   'overlay/xy2
	   (image=? (color-list->image (list red red red blue) 2 2 0 0)
		    (overlay/xy (p00 (rectangle 2 2 'solid 'red))
				   1 1
				   (p00 (rectangle 1 1 'solid 'blue)))))

(htdp-test #t
	   'overlay/xy3
	   (image=? (color-list->image (list red red blue blue) 2 2 0 0)
		    (overlay/xy (p00 (rectangle 2 1 'solid 'red))
				   0 1
				   (p00 (rectangle 2 1 'solid 'blue)))))

(htdp-test #t
	   'overlay/xy4
	   (image=? (color-list->image (list blue blue red red) 2 2 0 0)
		    (overlay/xy (p00 (rectangle 2 1 'solid 'red))
				   0 -1
				   (p00 (rectangle 2 1 'solid 'blue)))))

(htdp-test #t
           'overlay/xy/white
           (image=? (alpha-color-list->image (list ablack ablack ablack
                                                   ablack awhite ablack
                                                   ablack ablack ablack)
                                             3 3 0 0)
                    (overlay/xy (p00 (rectangle 3 3 'solid 'black))
                                   1 1
                                   (p00 (rectangle 1 1 'solid 'white)))))

(htdp-test #t
           'color-list->image/white-in-mask
           (image=? (color-list->image (list black red   black
                                             red   red   red
                                             black red   black)
                                       3 3 0 0)
                    (overlay (p00 (rectangle 3 3 'solid 'red))
                            (color-list->image (list black white black
                                                     white white white
                                                     black white black)
                                               3 3 0 0))))


(htdp-test #t
	   'overlay
	   (image=? (color-list->image (list red blue red red blue red) 3 2 0 0)
		    (overlay/xy (p00 (rectangle 3 2 'solid 'red))
				   1 0
				   (p00 (rectangle 1 2 'solid 'blue)))))

(htdp-test #t
	   'image-inside?1
	   (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
					 1 0
					 (p00 (rectangle 1 2 'solid 'blue)))
			  (rectangle 1 2 'solid 'blue)))

(htdp-test #f
	   'image-inside?2
	   (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
					 1 0
					 (p00 (rectangle 1 2 'solid 'blue)))
			  (rectangle 1 2 'solid 'black)))

(htdp-test #t
	   'image-inside?3
	   (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
					 1 0
					 (p00 (rectangle 1 2 'solid 'blue)))
			  (rectangle 1 2 'solid 'red)))

(htdp-test #f
	   'image-inside?4
	   (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
					 1 0
					 (p00 (rectangle 1 2 'solid 'blue)))
			  (rectangle 2 1 'solid 'red)))

(htdp-test #t
           'image-inside?5
           (image-inside? (alpha-color-list->image (list (make-alpha-color 0 255 0 0)) 1 1 0 0)
                          (alpha-color-list->image (list (make-alpha-color 255 0 0 0)) 1 1 0 0)))

(htdp-test #f
	   'image-inside?6
	   (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
					 1 0
					 (p00 (rectangle 1 2 'solid 'blue)))
			  (color-list->image (list blue white white) 
					     3 1 0 0)))

(htdp-test #t
	   'image-inside?7
	   (image-inside? (overlay/xy (p00 (rectangle 16 16 'solid 'red))
                                         2 5
                                         (p00 (ellipse 6 6 'outline 'blue)))
                          (ellipse 6 6 'outline 'blue)))

(htdp-test #t
           'image-inside?8
           (image-inside?
            (overlay (p00 (rectangle (image-width (text "x" 12 'red))
                                    (image-height (text "x" 12 'red))
                                    'solid 
                                    'white))
                    (text "x" 12 'red))
            (text "x" 12 'red)))

(htdp-test #t
           'image-inside?9
           (image-inside?
            (text "y x y" 12 'red)
            (text "x" 12 'red)))

(htdp-test (make-posn 2 5)
	   'find-image
	   (find-image (overlay/xy (p00 (rectangle 16 16 'solid 'red))
				      2 5
				      (p00 (ellipse 6 6 'outline 'blue)))
		       (ellipse 6 6 'outline 'blue)))

(htdp-test (make-posn 0 0)
	   'find-image
	   (find-image (rectangle 16 16 'solid 'blue)
		       (ellipse 6 6 'outline 'blue)))

(htdp-test 5
	   'image-width
	   (image-width (rectangle 5 7 'solid 'red)))

(htdp-test 7
	   'image-height
	   (image-height (rectangle 5 7 'solid 'red)))

(htdp-test 1 'color-red (color-red (make-color 1 2 3)))
(htdp-test 2 'color-green (color-green (make-color 1 2 3)))
(htdp-test 3 'color-blue (color-blue (make-color 1 2 3)))
(htdp-test #t 'color? (color? (make-color 1 2 3)))
(htdp-test #f 'color? (color? 10))

(htdp-test #t
           'line 
           (image=? (line 4 0 'red)
                    (color-list->image (list red red red red red) 5 1 0 0)))

(htdp-test #t
           'line 
           (image=? (line 0 4 'red)
                    (color-list->image (list red red red red red) 1 5 0 0)))

;; note: next two tests may be platform-specific... I'm not sure.
;; I developed them under macos x. -robby
(htdp-test #t
           'triangle1
           (image=? (triangle 3 'outline 'red)
                    (color-list->image 
                     (list white red   white
                           white red   white
                           red   white red
                           red   red   red)
                     3
                     4
                     0
                     0)))

(htdp-test #t
           'triangle2
           (image=? (triangle 3 'solid 'red)
                    (color-list->image 
                     (list white red   white
                           white red   white
                           red   red   red
                           red   red   red)
                     3
                     4
                     0
                     0)))

(htdp-test #t
           'add-line1
           (image=? (overlay (p00 (rectangle 5 4 'solid 'black))
                             (p00 (rectangle 1 4 'solid 'red)))
                    (add-line (p00 (rectangle 4 4 'solid 'black))
                              -1 0
                              -1 3
                              'red)))

(htdp-test #t
           'add-line2
           (image=? (overlay (p00 (rectangle 4 5 'solid 'black))
                             (p00 (rectangle 4 1 'solid 'red)))
                    (add-line (p00 (rectangle 4 4 'solid 'black))
                              0 -1
                              3 -1
                              'red)))

(check-on-bitmap 'solid-rect (htdp-eval (rectangle 2 2 'solid 'red)))
(check-on-bitmap 'outline-rect (htdp-eval (rectangle 2 2 'outline 'red)))
(check-on-bitmap 'solid-ellipse (htdp-eval (ellipse 2 4 'solid 'red)))
(check-on-bitmap 'outline-ellipse (htdp-eval (ellipse 2 4 'outline 'red)))
(check-on-bitmap 'solid-ellipse (htdp-eval (circle 4 'solid 'red)))
(check-on-bitmap 'outline-ellipse (htdp-eval (circle 4 'outline 'red)))
(check-on-bitmap 'solid-triangle (htdp-eval (triangle 10 'solid 'red)))
(check-on-bitmap 'outline-triangle (htdp-eval (triangle 10 'outline 'red)))
(check-on-bitmap 'line (htdp-eval (line 10 7 'red)))
(check-on-bitmap 'text (htdp-eval (text "XX" 12 'red)))
(check-on-bitmap 'overlay (htdp-eval (overlay (p00 (rectangle 1 4 'solid 'blue))
                                              (p00 (rectangle 4 1 'solid 'green)))))
(check-on-bitmap 'overlay (htdp-eval (overlay/xy (p00 (rectangle 4 4 'solid 'blue))
                                                 2 2
                                                 (p00 (rectangle 4 4 'solid 'green)))))
(check-on-bitmap 'alpha-color-list
                 (htdp-eval
                  (overlay
                   (p00 (rectangle 3 3 'solid 'blue))
                   (alpha-color-list->image
                    (list ared aclr ared
                          aclr aclr aclr
                          ared aclr ared)
                    3
                    3
                    0
                    0))))
(check-on-bitmap 'add-line
                 (htdp-eval
                  (add-line
                   (p00 (rectangle 100 100 'solid 'black))
                   -10 -10
                   110 110
                   'red)))

#|

The tests beginning with "bs-" ensure
that the operations all can accept bitmap 
snips as arguments

|#

(htdp-test #t
           'bs-image?
           (image? image-snip1))
(htdp-test #t
           'bs-image?
           (image? image-snip2))
(htdp-test #t
           'bs-image=?
           (image=? image-snip1 image-snip2))
(htdp-test 2
           'bs-image-width
           (image-width image-snip1))
(htdp-test 2
           'bs-image-width
           (image-width image-snip2))
(htdp-test 2
           'bs-image-height
           (image-height image-snip1))
(htdp-test 2
           'bs-image-height
           (image-height image-snip2))
(htdp-test #t
           'bs-overlay
           (image=? image-snip1 (overlay image-snip1 image-snip2)))
(htdp-test #t
           'bs-overlay/xy
           (image=? image-snip1 (overlay/xy image-snip1 0 0 image-snip2)))
(htdp-test #t
           'bs-add-line
           (image=?
            (add-line image-snip1 0 0 10 10 'green)
            (add-line image-snip2 0 0 10 10 'green)))
(htdp-test #t
           'bs-image-inside?1
           (image-inside? image-snip1 image-snip2))
(htdp-test #t
           'bs-image-inside?2
           (image-inside? image-snip1 image-snip2))
(htdp-test (make-posn 0 0)
           'bs-find-image1
           (find-image image-snip1 image-snip2))
(htdp-test (make-posn 0 0)
           'bs-find-image2
           (find-image image-snip2 image-snip1))
(htdp-test #t
           'bs-image->color-list
           (equal? (image->color-list image-snip1)
                   (image->color-list image-snip2)))
(htdp-test #t
           'bs-image->alpha-color-list
           (equal? (image->alpha-color-list image-snip1)
                   (image->alpha-color-list image-snip2)))

(report-errs)