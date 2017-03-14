#lang racket/base
(require (only-in mzlib/struct make-->vector)
         racket/class racket/draw)
(provide vec->struc
         (struct-out turtle) turtle->vector
         (struct-out offset) offset->vector
         (struct-out line) line->vector
         (struct-out tmerge) tmerge->vector
         (struct-out turtles/offset) turtles/offset->vector)

(define (vec->struc sexp)
  (cond
    [(pair? sexp) (cons (vec->struc (car sexp)) (vec->struc (cdr sexp)))]
    [(vector? sexp)
     (apply (case (vector-ref sexp 0)
              [(struct:turtle) make-turtle]
              [(struct:offset) make-offset]
              [(struct:line)
               (λ args
                 (define bad-color-line
                   (cond
                     [(= (length args) (procedure-arity make-line))
                      (apply make-line args)]
                     [(= (+ (length args) 1) (procedure-arity make-line))
                      (apply make-line (append args (list 1)))]))
                 (define bad-color (line-color bad-color-line))
                 (define the-color
                   (cond
                     [(list? bad-color)
                      (make-object color%
                        (list-ref bad-color 0)
                        (list-ref bad-color 1)
                        (list-ref bad-color 2)
                        (list-ref bad-color 3))]
                     ;; backwards compatibility cases (not
                     ;; sure if the #f case can actually happen)
                     [(equal? bad-color #t)
                      (send the-color-database find-color "black")]
                     [(equal? bad-color #f)
                      (send the-color-database find-color "white")]))
                 (struct-copy line bad-color-line [color the-color]))]
              [(struct:tmerge) make-tmerge]
              [(struct:turtles/offset) make-turtles/offset]
              [else (error 'vec->struc "unknown structure: ~s\n" sexp)])
            (map vec->struc (vector-ref sexp 1)))]
    [else sexp]))


;; a turtle is:
;; - (make-turtle x y theta)
;; where x, y, and theta are numbers
(define-struct turtle (x y angle))
(define turtle->vector (make-->vector turtle))

(define-struct offset (x y angle))
(define offset->vector (make-->vector offset))

;; a lines is:
;;   - (list-of line)
(define-struct line (x1 y1 x2 y2 color width))
(define pre-line->vector (make-->vector line))
(define (line->vector l)
  (define c (line-color l))
  (pre-line->vector
   (struct-copy line l [color (list (send c red)
                                    (send c green)
                                    (send c blue)
                                    (send c alpha))])))

;; a turtles is either
;;  - (make-tmerge (list-of turtles/offset))
;;  - (list-of turtle)
(define-struct tmerge (turtles))
(define tmerge->vector (make-->vector tmerge))

;; a turtles/offset is
;; - (make-turtles/offset turtles offset)
(define-struct turtles/offset (turtles offset))
(define turtles/offset->vector (make-->vector turtles/offset))
