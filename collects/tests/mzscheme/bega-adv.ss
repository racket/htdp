
(test 1 'quote '1)
(test (list 'quote 1) 'quote ''1)
(test "Hello" 'quote '"Hello")
(test (list 1 2) 'quote '(1 2))
(test (list 1 (list 2 "hi")) 'quote '(1 (2 "hi")))

(test 1 'qq `1)
(test '(1 2) 'qq `(1 2))
(test 7 'qq `,(+ 3 4))
(test '(1 3) 'qq `(1 ,(+ 1 2)))
(test '(99 88 77) 'qq `(,(* 11 9) ,(* 11 `8) ,`,(* 11 7)))
(test '(1 2 3 4) 'qq `(1 ,@(list 2 3) 4))
(test '(quasiquote 11) 'qq ``11)
(test '(quasiquote (unquote 11)) 'qq ``,11)
(test '(quasiquote (unquote 22)) 'qq ``,,(* 11 2))
(test '(quasiquote ((unquote-splicing (22)))) 'qq ``(,@(,@(list (* 11 2)))))

(syntax-test #'quasiquote)
(syntax-test #'`unquote)
(syntax-test #'`unquote-splicing)
(syntax-test #'`(unquote-splicing 10))

(syntax-test #'unquote)
(syntax-test #'(unquote))
(syntax-test #'(unquote 10))

(syntax-test #'unquote-splicing)
(syntax-test #'(unquote-splicing (list 10)))
(syntax-test #'((unquote-splicing (list 10))))

(err/rt-test `(,@4))
