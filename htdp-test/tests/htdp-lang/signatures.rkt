; Tests for signatures, specifically struct signatures
#lang racket/base

(require rackunit
         (only-in lang/private/teach
                  beginner-define-struct advanced-define-struct
                  signature
                  Integer Boolean)
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
