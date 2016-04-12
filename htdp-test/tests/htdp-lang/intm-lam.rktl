(htdp-err/rt-test (map (lambda (x y) (+ x y)) (list 2 3 4))
  (exn-type-and-msg
    exn:fail:contract?
    #rx"map : first argument must be a function that expects one argument, given"))
