#lang info

(define scribblings '(("htdp-langs.scrbl" (multi-page) (teaching -12 ("HtDP")))
                      ("htdp-ptr.scrbl" () (teaching -11))))

(define language-family (list (hash 'family "HtDP"
                                    'doc '(lib "scribblings/htdp-langs/htdp-langs.scrbl")
                                    'order -100)))
