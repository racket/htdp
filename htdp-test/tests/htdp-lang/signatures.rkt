; Tests for signatures, specifically struct signatures
#lang racket/base

(require rackunit
         (only-in lang/private/teach
                  beginner-define-struct advanced-define-struct
                  signature
                  Integer Natural Boolean True False
                  String Char Symbol
                  [EmptyList htdp:EmptyList]
                  ConsOf)
         deinprogramm/signature/signature)

(define-syntax say-no
  (syntax-rules ()
    ((_ ?body ...)
     (let/ec exit
       (call-with-signature-violation-proc
	(lambda (obj signature message blame)
	  (exit 'no))
	(lambda ()
	  ?body ...))))))

(define-syntax failed-signature
  (syntax-rules ()
    ((_ ?body ...)
     (let/ec exit
       (call-with-signature-violation-proc
	(lambda (obj signature message blame)
	  (exit signature))
	(lambda ()
	  ?body ...))))))

(define-syntax failed-signature-name
  (syntax-rules ()
    ((_ ?body ...)
     (let/ec exit
       (call-with-signature-violation-proc
	(lambda (obj signature message blame)
	  (exit (signature-name signature)))
	(lambda ()
	  ?body ...))))))

(check-equal? (say-no (apply-signature False #t)) 'no)
(check-equal? (apply-signature False #f) #f)

(check-equal? (say-no (apply-signature True #f)) 'no)
(check-equal? (apply-signature True #t) #t)

(check-equal? (say-no (apply-signature Integer 1/2)) 'no)
(check-pred (λ (v) (= v 20241017))
            (apply-signature Integer 20241017))

(check-equal? (say-no (apply-signature Natural -1)) 'no)
(check-pred zero? (apply-signature Natural 0))

(check-equal? (say-no (apply-signature htdp:EmptyList #t)) 'no)
(check-equal? (apply-signature htdp:EmptyList null) null)

(check-equal? (say-no (apply-signature (ConsOf False Natural) (cons #t 5))) 'no)
(check-equal? (say-no (apply-signature (ConsOf False Natural) (cons #f -1))) 'no)
(check-equal? (apply-signature (ConsOf False Natural) (cons #f 5)) (cons #f 5))

(check-equal? (say-no (apply-signature String #\h)) 'no)
(check-equal? (apply-signature String "hello") "hello")

(check-equal? (say-no (apply-signature Char "h")) 'no)
(check-equal? (apply-signature Char #\h) #\h)

(check-equal? (say-no (apply-signature Symbol 0)) 'no)
(check-equal? (apply-signature Symbol 'yes) 'yes)

; Need to test both beginner and advanced
(advanced-define-struct dillo (alive? weight))
(advanced-define-struct parrot (sentence weight))

(define d1 (make-dillo #f 10))

(check-eq? (say-no (apply-signature Dillo d1)) d1)
(check-eq? (say-no (apply-signature Dillo #f)) 'no)
(check-eq? (say-no (apply-signature Dillo (make-parrot "hello" 10))) 'no)
(check-eq? (say-no (apply-signature (DilloOf Boolean Integer) d1)) d1)
(check-eq? (say-no (apply-signature (DilloOf Boolean Integer) #f)) 'no)
(check-eq? (say-no (apply-signature (DilloOf Boolean Integer) (make-parrot "hello" 10))) 'no)

(check-eq? (failed-signature-name (apply-signature (DilloOf Boolean Integer) (make-dillo 10 10)))
           'Boolean)
(check-eq? (failed-signature-name (apply-signature (DilloOf Boolean Integer) (make-dillo #f #f)))
           'Integer)

(advanced-define-struct Snake (length width))

(define s1 (make-Snake 300 5))

(check-eq? (say-no (apply-signature Snake s1)) s1)
(check-eq? (say-no (apply-signature Snake #f)) 'no)

(beginner-define-struct empty-list ())

(define nil (make-empty-list))

(beginner-define-struct pare (kar kdr))

(define (kons a d) (make-pare a d))

(define (ListOf a)
  (signature
   (mixed EmptyList
          (PareOf a (ListOf a)))))

(define list123 (kons 1 (kons 2 (kons 3 nil))))

(check-equal? (say-no (apply-signature (ListOf Integer)
                                       list123))
              list123)
(check-equal? (say-no (apply-signature (ListOf Integer) #f))
              'no)

(check-eq? (say-no (apply-signature (ListOf Integer)
                                    (kons 1 (kons #f (kons 3 nil)))))
           'no)
