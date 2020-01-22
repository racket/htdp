; Extract a srcloc from an exception, provided the right continuation-mark keys are present.
#lang racket/base
(provide exn-srcloc continuation-marks-srcloc)

(require lang/private/continuation-mark-key
	 setup/collects)

; return srcloc associated with exception, in user program, or #f
(define (exn-srcloc exn)
  (if (exn:srclocs? exn)
      (let ([srclocs ((exn:srclocs-accessor exn) exn)])
	(and (pair? srclocs)
	     (car srclocs)))
      (continuation-marks-srcloc (exn-continuation-marks exn))))
    
(define (continuation-marks-srcloc marks)
  (let ([cms (continuation-mark-set->list marks teaching-languages-continuation-mark-key)])
    (cond
     [(not cms) '()]
     [(findf (lambda (mark)
	       (and mark
		    (let ([ppath (car mark)])
		      (or (and (path? ppath)
			       (not (let ([rel (path->collects-relative ppath)])
				      (and (pair? rel)
					   (eq? 'collects (car rel))
					   (or (equal? #"lang" (cadr rel))
					       (equal? #"deinprogramm" (cadr rel)))))))
			  (symbol? ppath)))))
	     cms)
      => (lambda (mark)
	   (apply (lambda (source line col pos span)
		    (make-srcloc source line col pos span))
		  mark))]
     (else #f))))
