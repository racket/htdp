#lang racket/base
(require (only-in mzlib/struct make-->vector))
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
              [(struct:line) make-line]
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
(define-struct line (x1 y1 x2 y2 black?))
(define line->vector (make-->vector line))

;; a turtles is either
;;  - (make-tmerge (list-of turtles/offset))
;;  - (list-of turtle)
(define-struct tmerge (turtles))
(define tmerge->vector (make-->vector tmerge))

;; a turtles/offset is
;; - (make-turtles/offset turtles offset)
(define-struct turtles/offset (turtles offset))
(define turtles/offset->vector (make-->vector turtles/offset))
