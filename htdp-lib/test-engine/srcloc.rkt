; Extract a srcloc from an exception, provided the right continuation-mark keys are present.
#lang racket/base
(provide exn-srcloc continuation-marks-srcloc)

(require setup/collects
         errortrace/marks-to-context)

; return srcloc associated with exception, in user program, or #f
(define (exn-srcloc exn)
  (if (exn:srclocs? exn)
      (let ([srclocs ((exn:srclocs-accessor exn) exn)])
	(and (pair? srclocs)
	     (car srclocs)))
      (continuation-marks-srcloc (exn-continuation-marks exn))))

(define (continuation-marks-srcloc marks)
  (cond
    (((or (errortrace-continuation-mark-set->context)
          continuation-mark-set->context)
      marks)
     => (lambda (cms)
          (findf (lambda (mark)
                   (and (srcloc? mark)
                        (let ([ppath (srcloc-source mark)])
                          (or (and (path? ppath)
                                   (not (let ([rel (path->collects-relative ppath)])
                                          (and (pair? rel)
                                               (eq? 'collects (car rel))
                                               (or (equal? #"lang" (cadr rel))
                                                   (equal? #"deinprogramm" (cadr rel)))))))
                              (symbol? ppath)))))
                 cms)))
    (else #f)))
