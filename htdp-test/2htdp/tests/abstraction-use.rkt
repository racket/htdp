;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname abstraction-use) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/abstraction)

;; -----------------------------------------------------------------------------
;; [Listof Number] -> [Listof Number]
;; map 

(check-expect (add1-to-all-even '(1 3 5 99 10)) '(1 3 5 99 11))

(define (add1-to-all-even.v1 l)
  (map (lambda (x) (if (even? x) (add1 x) x)) l))

(define (add1-to-all-even l)
  (for/list ([x l])
    (if (even? x) (add1 x) x)))

;; -----------------------------------------------------------------------------
;; [Listof X] [Listof Y] -> [Listof [List X Y]]
;; fold + map + map 

(define (length-is-9 l) 
  (= (length l) 9))

(check-satisfied (cross '(1 2 3) '(a b c)) length-is-9)

(define (cross.v1 i* j*)
  (foldl append '() (map (lambda (i) (map (lambda (j) (list i j)) j*)) i*)))

(define (cross ilist jlist)
  (for*/list ([i ilist][j jlist])
    (list i j)))

;; -----------------------------------------------------------------------------
;; N -> [Listof N]
;; build-list

(check-expect (count 3) '((0 0) (1 1) (2 2)))

(define (count n)
  (for/list ((i n))
    (list i i)))

;; -----------------------------------------------------------------------------
;; [Listof X] -> [Listof [List N X]]
;; enumerate

(check-expect (enumerate '(a b c)) '((1 a) (2 b) (3 c)))

(define (enumerate.v1 l)
  (map list (build-list (length l) add1) l))

(define (enumerate l)
  (for/list ((x l) (i (in-naturals 0)))
    (list (add1 i) x)))

;; -----------------------------------------------------------------------------

;; String -> String 

(check-expect (capitalize-every-other-letter "abcd") "AbCd")

(define (capitalize-every-other-letter s)
  (for/string ([c s][i (in-naturals 0)])
    (if (even? i) (string-upcase c) c)))

(define (string-upcase s)
  (string (char-upcase (first (string->list s)))))


;; -----------------------------------------------------------------------------
;; data abstraction 

(define-type BTree
  (leaf (info number?))
  (node (left BTree?) (right BTree?)))

(define btree0 (make-leaf 10))
(define btree1 (make-node btree0 btree0))
(define btree2 (make-node btree1 btree0))
(define btree3 (make-node btree1 btree1))

;; BTree -> Nat
;; determine the maximal nunber of nodes to be traversed to reach a leaf

(check-expect (depth btree0) 0)
(check-expect (depth btree1) 1)
(check-expect (depth btree2) 2)
(check-expect (depth btree3) 2)

(define (depth t)
  (type-case BTree t
    [leaf (info) 0]
    [node (left right) (+ (max (depth left) (depth right)) 1)]))


;; -----------------------------------------------------------------------------
;; [NEList-of X] -> X

(check-expect (last '(a b c)) 'c)
(check-expect (last '(1 2 3)) 3)
(check-error (last '()))

(define (last l)
  (match l
    [(cons x '()) x]
    [(cons fst rst) (last rst)]))

;; -----------------------------------------------------------------------------
;; Doll -> Symbol 

(define-struct doll (inside))
(check-expect (undress (make-doll 'wood)) 'wood)

(define (undress a-doll)
  (match a-doll
    [(struct doll (inside)) (undress inside)]
    [(? symbol?) a-doll]))
