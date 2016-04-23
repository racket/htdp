(htdp-err/rt-test (map (lambda (x y) (+ x y)) (list 2 3 4))
  (exn-type-and-msg
    exn:fail:contract?
    #rx"map: first argument must be a function that expects one argument"))

(htdp-err/rt-test (map (lambda (x y) (+ x y)) (list 2 3 4))
  (exn-type-and-msg
    exn:fail:contract?
    #rx"intm-lam.rktl"))

(htdp-err/rt-test (foldr (lambda (x y) (+ x y)) 0 (list 2 3 4) (list 2 3 4))
  (exn-type-and-msg
    exn:fail:contract?
    #rx"foldr: first argument must be a function that expects three arguments, given"))
