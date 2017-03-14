#lang racket/base
(require racket/math
         racket/class
         racket/draw
         racket/snip
         racket/list
         racket/format
         pict
         "value-turtles-reader.rkt"
         "value-turtles-wxme.rkt")

(provide turtles move draw turn turn/radians merge clean turtles?
         snip-class turtle-snip-class% turtle-state restore-turtle-state
         turtles-width turtles-height turtles-pict
         turtles-pen-width set-pen-width
         turtles-pen-color set-pen-color)

(define saved-turtle-snip% #f)
(define saved-turtles #f)

(define (struc->vec sexp)
  (cond
    [(pair? sexp) (cons (struc->vec (car sexp)) (struc->vec (cdr sexp)))]
    [(turtle? sexp) (vector 'struct:turtle (map struc->vec (vector->list (turtle->vector sexp))))]
    [(offset? sexp) (vector 'struct:offset (map struc->vec (vector->list (offset->vector sexp))))]
    [(line? sexp) (vector 'struct:line (map struc->vec (vector->list (line->vector sexp))))]
    [(tmerge? sexp) (vector 'struct:tmerge (map struc->vec (vector->list (tmerge->vector sexp))))]
    [(turtles/offset? sexp)
     (vector 'struct:turtles/offset (map struc->vec (vector->list (turtles/offset->vector sexp))))]
    [else sexp]))

(define prim-read read)
(define prim-write write)

(define turtle-snip-class%
  (class snip-class%
    (define/override (read in-stream)
      (unless saved-turtles
        (error 'turtles "click execute before running the turtles"))
      (let ([str (send in-stream get-unterminated-bytes #f)])
        (if (or (not str)
                (equal? #"" str))
            (saved-turtles 150 150)
            (let ([sexp (vec->struc (prim-read (open-input-bytes str)))])
              (cond
                [(= (length sexp) 5)
                 (make-object saved-turtle-snip%
                   (first sexp)
                   (second sexp)
                   (third sexp)
                   (fourth sexp)
                   (fifth sexp)
                   1
                   (send the-color-database find-color "black"))]
                [(= (length sexp) 6)
                 (make-object saved-turtle-snip%
                   (first sexp)
                   (second sexp)
                   (third sexp)
                   (fourth sexp)
                   (fifth sexp)
                   (sixth sexp)
                   (send the-color-database find-color "black"))]
                [(= (length sexp) 7)
                 (make-object saved-turtle-snip%
                   (first sexp)
                   (second sexp)
                   (third sexp)
                   (fourth sexp)
                   (fifth sexp)
                   (sixth sexp)
                   (to-color (seventh sexp)))])))))
    (super-instantiate ())))

(define (to-color lst) (make-object color% (first lst) (second lst) (third lst) (fourth lst)))

(define snip-class (make-object turtle-snip-class%))
(send snip-class set-classname (~s '((lib "value-turtles.rkt" "graphics" "private")
                                    (lib "value-turtles-wxme.rkt" "graphics" "private"))))

(define pi/2 (/ pi 2))
(define (set-box/f b v) (when (box? b) (set-box! b v)))
(define icon-color "PURPLE")
(define icon-pen (send the-pen-list find-or-create-pen icon-color 1 'xor))
(define icon-brush (send the-brush-list find-or-create-brush icon-color 'xor))
(define blank-pen (send the-pen-list find-or-create-pen "BLACK" 1 'transparent))
(define w-pen (send the-pen-list find-or-create-pen "white" 1 'solid))
(define b-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
(define w-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))

(define empty-cache (make-offset 0 0 0))
(define scroll-step 16)

(define turtle-snip%
  (class snip%
    (init-field width height turtles cache lines pen-width pen-color)
    (define/public (get-lines) lines)
    (define/public (get-turtles) turtles)
    (define/public (get-cache) cache)
    (define/public (get-width) width)
    (define/public (get-height) height)
    (define/public (get-pen-width) pen-width)
    (define/public (get-pen-color) pen-color)
    (define/public (set-pen-width pw)
      (make-object turtle-snip% width height turtles cache lines pw pen-color))
    (define/public (set-pen-color pc)
      (make-object turtle-snip% width height turtles cache lines pen-width pc))
    
    (define bitmap #f)

    (define/private (flip-icons dc dx dy)
      (define turtle-style 'triangle)
      (case turtle-style
        [(triangle line)
         (define proc
           (if (eq? turtle-style 'line)
               (λ (turtle)
                 (let ([x (turtle-x turtle)]
                       [y (turtle-y turtle)]
                       [theta (turtle-angle turtle)]
                       [size 2])
                   (send dc draw-line
                         (+ dx x)
                         (+ dy y)
                         (+ dx x (* size (cos theta)))
                         (+ dy y (* size (sin theta))))))
               (λ (turtle)
                 (let* ([x (turtle-x turtle)]
                        [y (turtle-y turtle)]
                        [theta (turtle-angle turtle)]
                        [long-size 20]
                        [short-size 7]
                        [l-theta (+ theta pi/2)]
                        [r-theta (- theta pi/2)])
                   (define pl (make-object point% 0 0))
                   (define pr (make-object point% 0 0))
                   (define ph (make-object point% 0 0))
                   (define points (list pl pr ph))
                   (send ph set-x (+ dx x (* long-size (cos theta))))
                   (send ph set-y (+ dy y (* long-size (sin theta))))
                   (send pl set-x (+ dx x (* short-size (cos l-theta))))
                   (send pl set-y (+ dy y (* short-size (sin l-theta))))
                   (send pr set-x (+ dx x (* short-size (cos r-theta))))
                   (send pr set-y (+ dy y (* short-size (sin r-theta))))
                   (send dc draw-polygon points)))))
         (define old-smoothing (send dc get-smoothing))
         (send dc set-smoothing 'aligned)
         (if (eq? turtle-style 'line)
             (send dc set-pen icon-pen)
             (begin
               (send dc set-pen blank-pen)
               (send dc set-brush icon-brush)))
         (for-each proc turtles)
         (send dc set-smoothing old-smoothing)
         (send dc set-pen b-pen)]
        [else
         (void)]))

    (define/public (to-pict)
      (flatten)

      (cond
        [(pair? lines)
         (define l (min (line-x1 (car lines)) (line-x2 (car lines))))
         (define r (max (line-x1 (car lines)) (line-x2 (car lines))))
         (define t (min (line-y1 (car lines)) (line-y2 (car lines))))
         (define b (max (line-y1 (car lines)) (line-y2 (car lines))))

         (for ([line (in-list lines)])
           (set! l (min l (line-x1 line) (line-x2 line)))
           (set! r (max r (line-x1 line) (line-x2 line)))
           (set! t (min t (line-y1 line) (line-y2 line)))
           (set! b (max b (line-y1 line) (line-y2 line))))

         (dc (λ (dc dx dy)
               (define pen (send dc get-pen))
               (for ([line (in-list lines)])
                 (send dc set-pen
                       (line-color line)
                       (min 255 (* (send pen get-width) (line-width line)))
                       'solid)
                 (send dc draw-line
                       (+ dx (- (line-x1 line) l))
                       (+ dy (- (line-y1 line) t))
                       (+ dx (- (line-x2 line) l))
                       (+ dy (- (line-y2 line) t))))
               (send dc set-pen pen))
             (- r l)
             (- b t))]
        [else (blank)]))
    
    (define/private (draw-in-dc dc dx dy)
      (flatten)
      (for ([line (in-list lines)])
        (send dc set-pen
              (line-color line)
              (line-width line)
              'solid)
        (send dc draw-line
              (+ dx (line-x1 line))
              (+ dy (line-y1 line))
              (+ dx (line-x2 line))
              (+ dy (line-y2 line)))))
      
    (define/private (construct-bitmap)
      (unless bitmap
        (set! bitmap (make-bitmap width height))
        (define bitmap-dc (make-object bitmap-dc% bitmap))
        (send bitmap-dc set-smoothing 'aligned)
        (send bitmap-dc clear)
        (draw-in-dc bitmap-dc 0 0)))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (construct-bitmap)
      (let ([old-pen (send dc get-pen)]
            [old-brush (send dc get-brush)]
            [old-clip (send dc get-clipping-region)])
        (send dc set-pen b-pen)
        (send dc set-brush w-brush)
        (send dc draw-rectangle x y (+ width 2) (+ height 2))
        (send dc set-clipping-rect (+ x 1) (+ y 1) width height)
        (send dc draw-bitmap bitmap (+ x 1) (+ y 1))
        (flip-icons dc (+ x 1) (+ y 1))
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)
        (send dc set-clipping-region old-clip)))
    (define/override (get-extent dc x y w h descent space lspace rspace)
      (set-box/f w (+ width 2))
      (set-box/f h (+ height 2))
      (set-box/f descent 0)
      (set-box/f space 0)
      (set-box/f lspace 0)
      (set-box/f rspace 0))

    (define/override (get-num-scroll-steps) (max 1 (inexact->exact (ceiling (/ height scroll-step)))))
    (define/override (find-scroll-step y) (inexact->exact (round (/ y scroll-step))))
    (define/override (get-scroll-step-offset offset) (* offset scroll-step))
    
    (define/override (copy)
      (make-object turtle-snip% width height turtles cache lines pen-width pen-color))
    (define/override (write stream-out)
      (define p (open-output-bytes))
      (prim-write (struc->vec (list width height turtles cache lines pen-width
                                    (list (send pen-color red)
                                          (send pen-color green)
                                          (send pen-color blue)
                                          (send pen-color alpha)))) p)
      (define btes (get-output-bytes p))
      (send stream-out put (bytes-length btes) btes))
    
    (define/public (flatten)
      (define (walk-turtles turtles offset sofar)
        (cond
          [(tmerge? turtles)
           (let ([turtles/offsets (tmerge-turtles turtles)]
                 [ac (apply-offset cache)])
             (foldl (lambda (turtles/offset sofar)
                      (walk-turtles (turtles/offset-turtles turtles/offset)
                                    (combine-offsets
                                     offset
                                     (turtles/offset-offset turtles/offset))
                                    sofar))
                    sofar
                    turtles/offsets))]
          [else
           (define f (apply-offset offset))
           (cond
             [(null? sofar)
              (map f turtles)]
             [else
              (foldl (lambda (t l) (cons (f t) l)) sofar turtles)])]))
      (set! turtles (walk-turtles turtles cache null))
      (set! cache empty-cache))
    
    (define/private (move-turtle dist)
      (lambda (turtle)
        (let ([x (turtle-x turtle)]
              [y (turtle-y turtle)]
              [theta (turtle-angle turtle)])
          (make-turtle
           (+ x (* dist (cos theta)))
           (+ y (* dist (sin theta)))
           theta))))
    
    (define/public (draw-op d)
      (flatten)
      (define (build-line turtle)
        (let ([x (turtle-x turtle)]
              [y (turtle-y turtle)]
              [theta (turtle-angle turtle)])
          (make-line
           x
           y
           (+ x (* d (cos theta)))
           (+ y (* d (sin theta)))
           pen-color
           pen-width)))
      (make-object turtle-snip%
        width
        height
        (map (move-turtle d) turtles)
        cache
        (foldl (lambda (turtle lines) (cons (build-line turtle) lines))
               lines
               turtles)
        pen-width
        pen-color))
    (define/public (merge-op tvs)
      (make-object turtle-snip%
        width
        height
        (make-tmerge (map (lambda (tv) (make-turtles/offset
                                        (send tv get-turtles)
                                        (send tv get-cache)))
                          (cons this tvs)))
        empty-cache
        lines
        pen-width
        pen-color))
    
    (define/public (move-op n)
      (make-object turtle-snip%
        width
        height
        turtles
        (let* ([angle (offset-angle cache)]
               [x (offset-x cache)]
               [y (offset-y cache)]
               [newx (+ x (* n (cos angle)))]
               [newy (+ y (* n (sin angle)))])
          (make-offset newx newy angle))
        lines
        pen-width
        pen-color))
    (define/public (turn-op d)
      (make-object turtle-snip%
        width
        height
        turtles
        (make-offset (offset-x cache)
                     (offset-y cache)
                     (- (offset-angle cache)
                        d))
        lines
        pen-width
        pen-color))
    (define/public (clean-op)
      (flatten)
      (make-object turtle-snip%
        width
        height
        null
        empty-cache
        lines
        pen-width
        pen-color))
    (super-new)
    (inherit set-snipclass)
    (set-snipclass snip-class)))

(define (apply-offset offset)
  (let ([x (offset-x offset)]
        [y (offset-y offset)]
        [offset-angle (offset-angle offset)])
    (lambda (turtle)
      (let* ([angle (turtle-angle turtle)])
        (let* ([c (cos angle)]
               [s (sin angle)]
               [rx (- (* x c) (* y s))]
               [ry (+ (* y c) (* x s))])
          (make-turtle (+ rx (turtle-x turtle))
                       (+ ry (turtle-y turtle))
                       (+ offset-angle angle)))))))

(define (combine-offsets offset1 offset2)
  (let ([answer ((apply-offset offset1)
                 (make-turtle
                  (offset-x offset2)
                  (offset-y offset2)
                  (offset-angle offset2)))])
    (make-offset
     (turtle-x answer)
     (turtle-y answer)
     (turtle-angle answer))))

(define turtles
  (case-lambda
    [(width height x y theta)
     (make-object turtle-snip%
       width height
       (list (make-turtle x y theta))
       empty-cache
       null
       1
       (send the-color-database find-color "black"))]
    [(width height)
     (turtles width height 
              (quotient width 2)
              (quotient height 2)
              0)]))

(define (turtles? x) (or (is-a? x turtle-snip%) (wxme-turtle? x)))

(define (move d tv) (send (wxme-turtle->snip turtle-snip% tv) move-op d))
(define (draw d tv) (send (wxme-turtle->snip turtle-snip% tv) draw-op d))
(define (turn/radians d tv) (send (wxme-turtle->snip turtle-snip% tv) turn-op d))
(define (turn d tv) (turn/radians (* (/ d 360) 2 pi) (wxme-turtle->snip turtle-snip% tv)))
(define (merge tv . tvs)
  (send (wxme-turtle->snip turtle-snip% tv) merge-op
        (for/list ([tv (in-list tvs)])
          (wxme-turtle->snip turtle-snip% tv))))
(define (clean tv) (send (wxme-turtle->snip turtle-snip% tv) clean-op))
(define (turtle-state tv)
  (define t (wxme-turtle->snip turtle-snip% tv))
  (send t flatten)
  (for/list ([t (in-list (send t get-turtles))])
    (vector-immutable (turtle-x t) (turtle-y t) (turtle-angle t))))
(define (restore-turtle-state _tv state)
  (define tv (wxme-turtle->snip turtle-snip% _tv))
  (define w (send tv get-width))
  (define h (send tv get-height))
  (apply
   merge
   (clean tv)
   (for/list ([s (in-list (reverse state))])
     (define x (vector-ref s 0))
     (define y (vector-ref s 1))
     (define θ (vector-ref s 2))
     (define w/x (move x (move (- (/ w 2)) (turtles w h))))
     (define w/y (turn -90 (move (- y) (move (/ h 2) (turn 90 w/x)))))
     (turn/radians (- θ) w/y))))
(define (turtles-width tv) (send (wxme-turtle->snip turtle-snip% tv) get-width))
(define (turtles-height tv) (send (wxme-turtle->snip turtle-snip% tv) get-height))
(define (turtles-pict tv) (send (wxme-turtle->snip turtle-snip% tv) to-pict))
(define (turtles-pen-width tv) (send (wxme-turtle->snip turtle-snip% tv) get-pen-width))
(define (turtles-pen-color tv) (send (wxme-turtle->snip turtle-snip% tv) get-pen-color))
(define (set-pen-width tv pw) (send (wxme-turtle->snip turtle-snip% tv) set-pen-width pw))
(define (set-pen-color tv _pc)
  (define pc
    (cond
      [(string? _pc)
       (or (send the-color-database find-color _pc)
           (send the-color-database find-color "black"))]
      [else _pc]))
  (send (wxme-turtle->snip turtle-snip% tv) set-pen-color pc))

(set! saved-turtle-snip% turtle-snip%)
(set! saved-turtles turtles)
