#|

The test suite for this code is in
plt/collects/tests/mzscheme/image-test.ss

|#

(module image mzscheme

  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "cache-image-snip.ss" "mrlib")
           (lib "math.ss")
	   (lib "posn.ss" "lang"))

  (provide image?
	   image=?
           
	   image-width
	   image-height
	   overlay
	   overlay/xy
           
           pinhole-x
           pinhole-y
           move-pinhole
           
           rectangle
           circle
           ellipse
           triangle
           line
           add-line
           text
           
	   image-inside?
	   find-image
           
	   image->color-list
	   color-list->image
           
           image->alpha-color-list
           alpha-color-list->image
           
           make-color
           color-red
           color-green
           color-blue
           color?
           make-alpha-color
           alpha-color-alpha
           alpha-color-red
           alpha-color-green
           alpha-color-blue
           alpha-color?
           color-list?
           alpha-color-list?)

  ;; ----------------------------------------
           
  (define (color-list? l)
      (and (list? l) (andmap color? l)))
  (define (alpha-color-list? l)
    (and (list? l) (andmap alpha-color? l)))

  (define-struct color (red green blue) (make-inspector))
  (define-struct alpha-color (alpha red green blue) (make-inspector))

  ;; ----------------------------------------

  (define (snip-size a)
    (cond
      [(is-a? a image-snip%)
       (let ([bm (send a get-bitmap)])
         (values (send bm get-width)
                 (send bm get-height)))]
      [(is-a? a cache-image-snip%)
       (send a get-size)]))
  
  (define (check name p? v desc)
    (unless (p? v)
      (raise-type-error
       name
       desc
       v)))
  
  ;; ----------------------------------------

  (define (image? a)
    (or (is-a? a image-snip%)
        (is-a? a cache-image-snip%)))

  ;; equal? is wrong -- if both images have an alpha of 255, 
  ;; the colors at that point are irrelevant
  ;; need to make sure there is a test case for that behavior
  (define (image=? a-raw b-raw)
    (unless (image? a-raw) (raise-type-error 'image=? "image" 0 a-raw b-raw))
    (unless (image? b-raw) (raise-type-error 'image=? "image" 1 a-raw b-raw))
    (let ([a (coerce-to-cache-image-snip a-raw)]
          [b (coerce-to-cache-image-snip b-raw)])
      (let-values ([(aw ah) (snip-size a)]
                   [(bw bh) (snip-size b)])
        (and (= aw bw)
             (= ah bh)
             (same/alpha? (argb-vector (send a get-argb))
                          (argb-vector (send b get-argb)))))))
  
  (define (same/alpha? v1 v2)
    (let loop ([i (vector-length v1)])
      (or (zero? i)
          (let ([a1 (vector-ref v1 (- i 4))]
                [a2 (vector-ref v2 (- i 4))])
            (or (= a1 a2 255)
                (and (= a1 a2)
                     (= (vector-ref v1 (- i 3)) (vector-ref v2 (- i 3)))
                     (= (vector-ref v1 (- i 2)) (vector-ref v2 (- i 2)))
                     (= (vector-ref v1 (- i 1)) (vector-ref v2 (- i 1)))
                     (loop (- i 4))))))))
      

  (define (image-width a)
    (check 'image-width image? a "image")
    (let-values ([(w h) (snip-size a)])
      (inexact->exact (ceiling w))))

  (define (image-height a)
    (check 'image-height image? a "image")
    (let-values ([(w h) (snip-size a)])
      (inexact->exact (ceiling h))))
  
  (define (pinhole-x a)
    (check 'pinhole-x image? a "image")
    (let-values ([(x y) (send (coerce-to-cache-image-snip a) get-pinhole)])
      x))
  
  (define (pinhole-y a)
    (check 'pinhole-y image? a "image")
    (let-values ([(x y) (send (coerce-to-cache-image-snip a) get-pinhole)])
      y))
  
  (define (move-pinhole raw-i dx dy)
    (check 'move-pinhole image? raw-i "image")
    (check 'move-pinhole number? dx "number")
    (check 'move-pinhole number? dy "number")
    (let ([i (coerce-to-cache-image-snip raw-i)])
      (let-values ([(px py) (send i get-pinhole)]
                   [(w h) (send i get-size)])
        (new cache-image-snip%
             (dc-proc (send i get-dc-proc))
             (argb-proc (send i get-argb-proc))
             (width w)
             (height h)
             (argb (send i get-argb))
             (px (+ px dx))
             (py (+ py dy))))))
        
  (define (overlay a b . cs)
    (check 'overlay image? a "image")
    (check 'overlay image? b "image")
    (let ([all-imgs (reverse (list* a b cs))])
      (let loop ([imgs (cdr all-imgs)]
                 [acc (car all-imgs)])
        (cond
          [(null? imgs) acc]
          [else (loop (cdr imgs)
                      (real-overlay/xy 'overlay (car imgs) 0 0 acc))]))))

  (define (overlay/xy a dx dy b)
    (check 'overlay/xy image? a "image")
    (check 'overlay/xy real? dx "real number")
    (check 'overlay/xy real? dy "real number")
    (check 'overlay/xy image? b "image")
    (real-overlay/xy 'overlay/xy a dx dy b))

  (define (real-overlay/xy name raw-a raw-delta-x raw-delta-y raw-b)
    (let ([a (coerce-to-cache-image-snip raw-a)]
          [b (coerce-to-cache-image-snip raw-b)])
      (let-values ([(a-w a-h) (snip-size a)]
                   [(b-w b-h) (snip-size b)]
                   [(a-px a-py) (send a get-pinhole)]
                   [(b-px b-py) (send b get-pinhole)])
        (let* ([delta-x (+ raw-delta-x a-px (- b-px))]
               [delta-y (+ raw-delta-y a-py (- b-py))]
               [left (min 0 delta-x)]
               [top (min 0 delta-y)]
               [right (max (+ delta-x b-w) a-w)]
               [bottom (max (+ delta-y b-h) a-h)]
               [new-w (ceiling (- right left))]
               [new-h (ceiling (- bottom top))]
               [a-dx (- left)]
               [a-dy (- top)]
               [b-dx (- delta-x left)]
               [b-dy (- delta-y top)]
               [new-px (- a-px left)]
               [new-py (- a-py top)]
               [combine (lambda (a-f b-f)
                          (lambda (dc dx dy)
                            (a-f dc (+ dx a-dx) (+ dy a-dy))
                            (b-f dc (+ dx b-dx) (+ dy b-dy))))])
          (check-sizes name new-w new-h)
          (new cache-image-snip%
               [dc-proc (combine (send a get-dc-proc)
                                 (send b get-dc-proc))]
               [argb-proc (combine (send a get-argb-proc)
                                   (send b get-argb-proc))]
               [width new-w]
               [height new-h]
               [px new-px]
               [py new-py])))))
  
  ;; coerce-to-cache-image-snip : image -> (is-a?/c cache-image-snip%)
  (define (coerce-to-cache-image-snip snp)
    (cond
      [(is-a? snp image-snip%)
       (let* ([bmp (send snp get-bitmap)]
              [bmp-mask (or (send bmp get-loaded-mask)
                            (send snp get-bitmap-mask)
                            (bitmap->mask bmp))])
         (bitmaps->cache-image-snip (copy-bitmap bmp)
                                    (copy-bitmap bmp-mask)
                                    (floor (/ (send bmp get-width) 2))
                                    (floor (/ (send bmp get-height) 2))))]
      [else snp]))
    
  ;; copy-bitmap : bitmap -> bitmap
  ;; does not copy the mask.
  (define (copy-bitmap bitmap)
    (let* ([w (send bitmap get-width)]
           [h (send bitmap get-height)]
           [copy (make-object bitmap% w h)]
           [a-dc (make-object bitmap-dc% copy)])
      (send a-dc clear)
      (send a-dc draw-bitmap bitmap 0 0)
      (send a-dc set-bitmap #f)
      copy))
  
  ;; bitmap->mask : bitmap -> bitmap
  (define (bitmap->mask bitmap)
    (let* ([w (send bitmap get-width)]
           [h (send bitmap get-height)]
           [s (make-string (* 4 w h))]
           [new-bitmap (make-object bitmap% w h)]
           [dc (make-object bitmap-dc% new-bitmap)])
      (send dc clear)
      (send dc draw-bitmap bitmap 0 0)
      (send dc get-argb-pixels 0 0 w h s)
      (let loop ([i (* 4 w h)])
        (unless (zero? i)
          (let ([r (- i 3)]
                [g (- i 2)]
                [b (- i 1)])
            (unless (and (eq? #\377 (string-ref s r))
                         (eq? #\377 (string-ref s g))
                         (eq? #\377 (string-ref s b)))
              (string-set! s r #\000)
              (string-set! s g #\000)
              (string-set! s b #\000))
            (loop (- i 4)))))
      (send dc set-argb-pixels 0 0 w h s)
      (begin0
        (send dc get-bitmap)
        (send dc set-bitmap #f))))

  ;; ------------------------------------------------------------

  (define (line x y color)
    (check-sizes 'line (+ x 1) (+ y 1))
    (let ([draw-proc (make-color-wrapper/check
                      'line color 'transparent 'solid
                      (lambda (dc dx dy)
                        (send dc draw-line dx dy (+ dx x) (+ dy y))))]
          [mask-proc
           (make-color-wrapper/check
            'line 'black 'transparent 'solid
            (lambda (dc dx dy)
              (send dc draw-line dx dy (+ dx x) (+ dy y))))])
      (make-simple-cache-image-snip (+ x 1) (+ y 1) 0 0 draw-proc mask-proc)))
  
  ;; test what happens when the line moves out of the box.
  (define (add-line raw-i pre-x1 pre-y1 pre-x2 pre-y2 color-in)
    (check 'add-line image? raw-i "image")
    (check 'add-line number? pre-x1 "number")
    (check 'add-line number? pre-y1 "number")
    (check 'add-line number? pre-x2 "number")
    (check 'add-line number? pre-y2 "number")
    (let ([i (coerce-to-cache-image-snip raw-i)])
      (let-values ([(px py) (send i get-pinhole)]
                   [(iw ih) (send i get-size)]
                   [(x1 y1 x2 y2)
                    (if (<= pre-x1 pre-x2)
                        (values pre-x1 pre-y1 pre-x2 pre-y2)
                        (values pre-x2 pre-y2 pre-x1 pre-y1))])
        (let* ([line-w (+ (abs (- x2 x1)) 1)]
               [line-h (+ (abs (- y2 y1)) 1)])
          (if (y1 . <= . y2)
              (let* ([do-draw
                      (lambda (dc dx dy)
                        (send dc draw-line 
                              dx
                              dy
                              (+ dx (- x2 x1))
                              (+ dy (- y2 y1))))]
                     [draw-proc 
                      (make-color-wrapper/check 'add-line color-in 'transparent 'solid do-draw)]
                     [mask-proc
                      (make-color-wrapper/check 'add-line 'black 'transparent 'solid do-draw)]
                     [line
                      (make-simple-cache-image-snip line-w line-h px py draw-proc mask-proc)])
                (real-overlay/xy 'add-line i (+ px x1) (+ py y1) line))
              (let* ([do-draw
                      (lambda (dc dx dy)
                        (send dc draw-line 
                              dx
                              (+ dy (- line-h 1))
                              (+ dx (- line-w 1))
                              dy))]
                     [draw-proc 
                      (make-color-wrapper/check 'add-line color-in 'transparent 'solid do-draw)]
                     [mask-proc
                      (make-color-wrapper/check 'add-line 'black 'transparent 'solid do-draw)]
                     [line
                      (make-simple-cache-image-snip line-w line-h px py draw-proc mask-proc)])
                (real-overlay/xy 'add-line i (+ px x1) (+ py y2) line)))))))

  (define (text str size color-in)
    (check 'text string? str "string")
    (check 'text (lambda (x) (and (integer? x) (<= 1 x 255))) size "integer between 1 and 255")
    (let ([color (make-color% color-in)])
      (unless color
        (error 'text "expected a color symbol as second argument, given: ~e" color))
      (let-values ([(tw th) (get-text-size size str)])
        (let ([draw-proc
               (lambda (txt-color mode dc dx dy)
                 (let ([old-mode (send dc get-text-mode)]
                       [old-fore (send dc get-text-foreground)]
                       [old-font (send dc get-font)])
                   (send dc set-text-mode mode)
                   (send dc set-text-foreground txt-color)
                   (send dc set-font (get-font size))
                   (send dc draw-text str dx dy)
                   (send dc set-text-mode old-mode)
                   (send dc set-text-foreground old-fore)
                   (send dc set-font old-font)))])
          (new cache-image-snip%
               [dc-proc (lambda (dc dx dy) (draw-proc color 'transparent dc dx dy))]
               [argb-proc 
                (lambda (argb dx dy)
                  (let ([bm-color
                         (build-bitmap
                          (lambda (dc)
                            (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
                            (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))
                            (send dc draw-rectangle 0 0 tw th))
                          tw
                          th)]
                        [bm-mask
                         (build-bitmap
                          (lambda (dc)
                            (draw-proc 
                             (send the-color-database find-color "black")
                             'solid dc 0 0))
                          tw
                          th)])
                    (overlay-bitmap argb dx dy bm-color bm-mask)))]
               [width tw]
               [height th]
               [px 0]
               [py 0])))))
                    
  (define (get-text-size size string)
    (let* ([bm (make-object bitmap% 1 1)]
           [dc (make-object bitmap-dc% bm)])
      (let-values ([(w h _1 _2) (send dc get-text-extent string (get-font size))])
        (values (inexact->exact (ceiling w)) 
                (inexact->exact (ceiling h))))))
  
  (define (get-font size)
    (send the-font-list find-or-create-font size
          'default 'normal 'normal #f
          (case (system-type)
            [(macosx) 'partly-smoothed]
            [else 'smoothed])))

  (define (a-rect/circ who do-draw w h color brush pen)
    (check-sizes who w h)
    (let* ([dc-proc (make-color-wrapper/check who color brush pen do-draw)]
           [mask-proc (make-color-wrapper/check who 'black brush pen do-draw)])
      (make-simple-cache-image-snip w h (floor (/ w 2)) (floor (/ h 2)) dc-proc mask-proc)))
  
  (define (rectangle w h mode color)
    (check 'rectangle mode? mode mode-str)
    (a-rect/circ 'rectangle
                 (lambda (dc dx dy) (send dc draw-rectangle dx dy w h))
                 w h color (mode->brush-symbol mode) (mode->pen-symbol mode)))
  
  (define (ellipse w h mode color)
    (check 'ellipse mode? mode mode-str)
    (a-rect/circ 'ellipse
                 (lambda (dc dx dy) (send dc draw-ellipse dx dy w h))
                 w h color (mode->brush-symbol mode) (mode->pen-symbol mode)))
  
  (define (circle r mode color)
    (check 'ellipse mode? mode mode-str)
    (a-rect/circ 'circle
                 (lambda (dc dx dy) (send dc draw-ellipse dx dy (* 2 r) (* 2 r)))
                 (* 2 r) (* 2 r) color (mode->brush-symbol mode) (mode->pen-symbol mode)))
  
  (define (triangle size mode color)
    (check 'triangle
           (lambda (x) (and (real? x) (< 2 x 10000)))
           size 
           "positive integer bigger than 2")
    (check 'triangle mode? mode mode-str)
    (let* ([right (- size 1)]
           [bottom (inexact->exact (ceiling (* size (sin (* 2/3 pi)))))]
           [points (list (make-object point% 0 bottom)
                         (make-object point% right bottom)
                         (make-object point% (/ size 2) 0))])
      (let ([draw (make-color-wrapper/check 
                   'triangle color (mode->brush-symbol mode) 'solid
                   (lambda (dc dx dy)
                     (send dc draw-polygon points dx dy)))]
            [mask-draw (make-color-wrapper/check 
                        'triangle 'black (mode->brush-symbol mode) 'solid
                        (lambda (dc dx dy)
                          (send dc draw-polygon points dx dy)))]
            [w size]
            [h (+ bottom 1)])
        (make-simple-cache-image-snip w h (floor (/ w 2)) (floor (/ h 2)) draw mask-draw))))
        
  (define (make-simple-cache-image-snip w h px py dc-proc mask-proc)
    (let ([argb-proc 
           (lambda (argb-vector dx dy)
             (let ([c-bm (build-bitmap (lambda (dc) (dc-proc dc 0 0)) w h)]
                   [m-bm (build-bitmap (lambda (dc) (mask-proc dc 0 0)) w h)])
               (overlay-bitmap argb-vector dx dy c-bm m-bm)))])
      (new cache-image-snip%
           [dc-proc dc-proc]
           [argb-proc argb-proc]
           [width w]
           [height h]
           [px px]
           [py py])))
  
  (define (mode? x)
    (or (eq? x 'solid)
        (eq? x 'outline)))
  
  (define mode-str "'solid or 'outline")
  
  (define (mode->brush-symbol m)
    (case m
      [(solid) 'solid]
      [(outline) 'transparent]))
  
  (define (mode->pen-symbol m)
    (case m
      [(solid) 'transparent]
      [(outline) 'solid]))
  
  (define (make-color-wrapper/check who color-in brush pen rest)
    (let ([color (make-color% color-in)])
      (unless color
        (error who "expected color, given: ~e" color-in))
      (lambda (dc dx dy)
        (let ([old-brush (send dc get-brush)]
              [old-pen (send dc get-pen)])
          (send dc set-brush (send the-brush-list find-or-create-brush color brush))
          (send dc set-pen (send the-pen-list find-or-create-pen color 1 pen))
          (rest dc dx dy)
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)))))
  
  (define (make-color% c)
    (cond
      [(string? c) (send the-color-database find-color c)]
      [(symbol? c) (send the-color-database find-color (symbol->string c))]
      [(color? c) (make-object color%
                    (color-red c)
                    (color-green c)
                    (color-blue c))]
      [else #f]))
      
  (define (check-sizes who w h)
    (check who real? w "positive integer")
    (check who real? h "positive integer")
    (unless (and (< 0 w 10000) (< 0 h 10000))
      (error (format "cannot make ~a x ~a image" w h))))
      
  ;; ------------------------------------------------------------

  (define (image-inside? i a)
    (and (locate-image 'image-inside? 
                       (coerce-to-cache-image-snip i)
                       (coerce-to-cache-image-snip a))
         #t))
  
  (define (find-image i a)
    (or (locate-image 'find-image 
                      (coerce-to-cache-image-snip i)
                      (coerce-to-cache-image-snip a))
	(error 'find-image
	       "the second image does not appear within the first image")))
  
  (define (locate-image who i a)
    (check who image? i "image")
    (check who image? a "image")
    (let-values ([(iw ih) (snip-size i)]
                 [(aw ah) (snip-size a)])
      (and (iw . >= . aw)
           (ih . >= . ah)
           (let ([i-argb-vector (argb-vector (send i get-argb))]
                 [a-argb-vector (argb-vector (send a get-argb))])
             (let ([al (let loop ([offset 0])
                         (cond
                           [(= offset (* ah aw 4)) null]
                           [else (cons (subvector a-argb-vector offset (+ offset (* 4 aw)))
                                       (loop (+ offset (* 4 aw))))]))])
               (let yloop ([dy 0])
                 (and (dy . <= . (- ih ah))
                      (let xloop ([dx 0])
                        (if (dx . <= . (- iw aw))
                            (if (let loop ([al al][dd 0])
                                  (or (null? al)
                                      (and (first-in-second?
                                            i-argb-vector 
                                            (car al)
                                            (* 4 (+ (* (+ dy dd) iw) dx)))
                                           (loop (cdr al) (add1 dd)))))
                                (make-posn dx dy)
                                (xloop (add1 dx)))
                            (yloop (add1 dy)))))))))))
  
  (define (subvector orig i j)
    (let ([v (make-vector (- j i) #f)])
      (let loop ([x i])
        (when (< x j)
          (vector-set! v (- x i) (vector-ref orig x))
          (loop (+ x 1))))
      v))
#|
(initial inequalities thanks to Matthew (thanks!!))

We know that, for a combination:
  m3 = (m1+m2-m1*m2) and 
  b3 = (m1*b1*(1-m2) + m2*b2)/m3

So, we need to figure out what m1 & m2 might have been, 
given the other values.

Check m3:

   m3 = m2 when m1 = 0
   m3 = 1 when m1 = 1

   [deriv of m3 with respect to m1 = 1 - m2, which is positive]

    so check that m3 is between m2 and 1

Then check m3*b3:

   b3*m3 = m2*b2  when m1 = 0 or b1 = 0
   b3*m3 = (1 - m2) + m2*b2 when m1 = b1 = 1

   [deriv with respect to m1 is b1*(1-m2), which is positive]
   [deriv with respect to b1 is m1*(1-m2), which is positive]

    So check that m3*b3 is between m2*b2 and (1 - m2) + m2*b2

This is all in alphas from 0 to 1 and needs to be from 255 to 0.
Converting (but using the same names) for the alpha test, we get:

(<= (- 1 (/ m2 255))
    (- 1 (/ m3 255))
    1)

sub1 to each:

(<= (- (/ m2 255))
    (- (/ m3 255))
    0)

mult by 255:

(<= (- m2)
    (- m3)
    0)

negate and flip ineq:


(>= m2 m3 0)

flip ineq back:

(<= 0 m3 m2)


Here's the original scheme expression for the second check:

(<= (* m2 b2) 
    (* m3 b3)
    (+ (- 1 m2) (* m2 b2))

converting from the computer's coordinates, we get:


(<= (* (- 1 (/ m2 255)) (- 1 (/ b2 255)))
    (* (- 1 (/ m3 255)) (- 1 (/ b3 255)))
    (+ (- 1 (- 1 (/ m2 255)))
       (* (- 1 (/ m2 255)) (- 1 (/ b2 255)))))

;; multiplying out the binomials:

(<= (+ 1
       (- (/ m2 255)) 
       (- (/ b2 255)) 
       (/ (* m2 b2) (* 255 255)))
    (+ 1
       (- (/ m3 255)) 
       (- (/ b3 255)) 
       (/ (* m3 b3) (* 255 255)))
    (+ (- 1 (- 1 (/ m2 255)))
       (+ 1
          (- (/ m2 255)) 
          (- (/ b2 255)) 
          (/ (* m2 b2) (* 255 255)))))

;; simplifying the last term
  
(<= (+ 1
       (- (/ m2 255)) 
       (- (/ b2 255)) 
       (/ (* m2 b2) (* 255 255)))
    (+ 1
       (- (/ m3 255)) 
       (- (/ b3 255)) 
       (/ (* m3 b3) (* 255 255)))
    (+ 1
       (- (/ b2 255)) 
       (/ (* m2 b2) (* 255 255))))
  
;; multiply thru by 255:

(<= (+ 255
       (- m2) 
       (- b2) 
       (* m2 b2 1/255))
    (+ 255
       (- m3)
       (- b3)
       (* m3 b3 1/255))
    (+ 255
       (- b2) 
       (* m2 b2 1/255)))
  
;; subtract out 255 from each:

(<= (+ (- m2) 
       (- b2) 
       (* m2 b2 1/255))
    (+ (- m3)
       (- b3)
       (* m3 b3 1/255))
    (+ (- b2) 
       (* m2 b2 1/255)))

;; negate them all, and reverse the inequality

(>= (+ m2 b2 (* m2 b2 -1/255))
    (+ m3 b3 (* m3 b3 -1/255))
    (+ b2 (* m2 b2 -1/255)))

;; aka

(<= (+ b2 (* m2 b2 -1/255))
    (+ m3 b3 (* m3 b3 -1/255))
    (+ m2 b2 (* m2 b2 -1/255)))

|#
  
  ;; in the above, m3 & b3 come from iv
  ;; and m2 & b2 come from av
  (define (first-in-second? iv av xd)
    (let loop ([i (vector-length av)])
      (or (zero? i)
	  (let ([a (- i 4)]
                [r (- i 3)]
		[g (- i 2)]
		[b (- i 1)])
            (let* ([m2 (vector-ref av a)]
                   [m3 (vector-ref iv (+ xd a))]
                   [check
                    (lambda (b2 b3)
                      (<= (+ b2 (* m2 b2 -1/255))
                          (+ m3 b3 (* m3 b3 -1/255))
                          (+ m2 b2 (* m2 b2 -1/255))))])
              (and (<= 0 m3 m2)
                   (check (vector-ref av r) (vector-ref iv (+ xd r)))
                   (check (vector-ref av g) (vector-ref iv (+ xd g)))
                   (check (vector-ref av b) (vector-ref iv (+ xd b)))
                   (loop (- i 4))))))))

  ;; ----------------------------------------

  (define (image->color-list i-raw)
    (check 'image->color-list image? i-raw "image")
    (let* ([cis (coerce-to-cache-image-snip i-raw)]
           [i (send cis get-bitmap)]
           [iw (send i get-width)]
           [ih (send i get-height)]
           [new-bitmap (make-object bitmap% iw ih)]
           [bdc (make-object bitmap-dc% new-bitmap)])
      (send bdc clear)
      (send bdc draw-bitmap i 0 0 'solid 
            (send the-color-database find-color "black")
            (send i get-loaded-mask))
      (let ([is (make-string (* 4 iw ih))]
            [cols (make-vector (* iw ih))])
        (send bdc get-argb-pixels 0 0 iw ih is)
        (let yloop ([y 0][pos 0])
          (unless (= y ih)
            (let xloop ([x 0][pos pos])
              (if (= x iw)
                  (yloop (add1 y) pos)
                  (begin
                    (vector-set! cols (+ x (* y iw))
                                 (make-color (char->integer (string-ref is (+ 1 pos)))
                                             (char->integer (string-ref is (+ 2 pos)))
                                             (char->integer (string-ref is (+ 3 pos)))))
                    (xloop (add1 x) (+ pos 4)))))))
        (send bdc set-bitmap #f)
        (vector->list cols))))
  
  (define (image->alpha-color-list i)
    (check 'image->alpha-color-list image? i "image")
    (let* ([argb (cond
                   [(is-a? i image-snip%) 
                    (send (coerce-to-cache-image-snip i) get-argb)]
                   [(is-a? i cache-image-snip%) (send i get-argb)])]
           [v (argb-vector argb)])
      (let loop ([i (vector-length v)]
                 [a null])
        (cond
          [(zero? i) a]
          [else (loop (- i 4)
                      (cons (make-alpha-color
                             (vector-ref v (- i 4))
                             (vector-ref v (- i 3))
                             (vector-ref v (- i 2))
                             (vector-ref v (- i 1)))
                            a))]))))
  
  (define (posi? i)
    (and (number? i) (integer? i) (positive? i) (exact? i)))

  (define (color-list->image cl w h px py)
    (check 'color-list->image color-list? cl "list-of-colors")
    (check 'color-list->image posi? w "positive exact integer")
    (check 'color-list->image posi? h "positive exact integer")
    (unless (and (< 0 w 10000) (< 0 h 10000))
      (error (format "cannot make ~a x ~a image" w h)))
    (unless (= (* w h) (length cl))
      (error (format "given width times given height is ~a, but the given color list has ~a items"
		     (* w h) (length cl))))
    (let* ([bm (make-object bitmap% w h)]
           [mask-bm (make-object bitmap% w h)]
	   [dc (make-object bitmap-dc% bm)]
           [mask-dc (make-object bitmap-dc% mask-bm)])
      (unless (send bm ok?)
	(error (format "cannot make ~a x ~a image" w h)))
      (let ([is (make-string (* 4 w h) #\000)]
            [mask-is (make-string (* 4 w h) #\000)]
	    [cols (list->vector cl)])
	(let yloop ([y 0][pos 0])
	  (unless (= y h)
	    (let xloop ([x 0][pos pos])
	      (if (= x w)
		  (yloop (add1 y) pos)
		  (let* ([col (vector-ref cols (+ x (* y w)))]
                         [r (pk color-red col)]
                         [g (pk color-green col)]
                         [b (pk color-blue col)])
		    (string-set! is (+ 1 pos) r)
		    (string-set! is (+ 2 pos) g)
		    (string-set! is (+ 3 pos) b)
                    (when (char=? r g b #\377)
                      (string-set! mask-is (+ 1 pos) #\377)
                      (string-set! mask-is (+ 2 pos) #\377)
                      (string-set! mask-is (+ 3 pos) #\377))
                    (xloop (add1 x) (+ pos 4)))))))
	(send dc set-argb-pixels 0 0 w h is)
        (send mask-dc set-argb-pixels 0 0 w h mask-is))
      (send dc set-bitmap #f)
      (send mask-dc set-bitmap #f)
      (bitmaps->cache-image-snip bm mask-bm px py)))
  
  (define (pk sel col) (integer->char (min 255 (max 0 (sel col)))))
  
  (define (bitmaps->cache-image-snip color mask px py)
    (let ([w (send color get-width)]
          [h (send color get-height)])
      (new cache-image-snip%
           [width w]
           [height h]
           [dc-proc
            (lambda (dc dx dy)
              (send dc draw-bitmap color 0 0 'solid 
                    (send the-color-database find-color "black")
                    mask))]
           [argb-proc
            (lambda (argb-vector dx dy)
              (overlay-bitmap argb-vector dx dy color mask))]
           [px px]
           [py py])))
              
  (define (alpha-color-list->image cl w h px py)
    (check 'alpha-color-list->image alpha-color-list? cl "list-of-alpha-colors")
    (check 'alpha-color-list->image posi? w "positive exact integer")
    (check 'alpha-color-list->image posi? h "positive exact integer")
    (unless (and (< 0 w 10000) (< 0 h 10000))
      (error (format "cannot make ~a x ~a image" w h)))
    (unless (= (* w h) (length cl))
      (error (format "given width times given height is ~a, but the given color list has ~a items"
		     (* w h) (length cl))))
    (let ([index-list (alpha-colors->ent-list cl)])
      (argb->cache-image-snip (make-argb (list->vector index-list) w) px py)))
  
  ;; alpha-colors->ent-list : (listof alpha-color) -> (listof number)
  (define (alpha-colors->ent-list cl)
    (let loop ([cl cl])
      (cond
        [(null? cl) null]
        [else 
         (let ([ac (car cl)])
           (list* (alpha-color-alpha ac)
                  (alpha-color-red ac)
                  (alpha-color-green ac)
                  (alpha-color-blue ac)
                  (loop (cdr cl))))]))))
