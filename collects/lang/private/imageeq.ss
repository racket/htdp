(module imageeq mzscheme
  (require (lib "mred.ss" "mred")
           (lib "cache-image-snip.ss" "mrlib")
           (lib "class.ss"))
  
  (provide image? image=? 
           coerce-to-cache-image-snip
           snip-size
           bitmaps->cache-image-snip)

  (define (image? a)
    (or (is-a? a image-snip%)
        (is-a? a cache-image-snip%)))
  
  (define (snip-size a)
    (cond
      [(is-a? a image-snip%)
       (let ([bm (send a get-bitmap)])
         (values (send bm get-width)
                 (send bm get-height)))]
      [(is-a? a cache-image-snip%)
       (send a get-size)]))
  
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
  
  (define (bitmaps->cache-image-snip color mask px py)
    (let ([w (send color get-width)]
          [h (send color get-height)])
      (new cache-image-snip%
           [width w]
           [height h]
           [dc-proc
            (lambda (dc dx dy)
              (send dc draw-bitmap color dx dy 'solid 
                    (send the-color-database find-color "black")
                    mask))]
           [argb-proc
            (lambda (argb-vector dx dy)
              (overlay-bitmap argb-vector dx dy color mask))]
           [px px]
           [py py]))))
