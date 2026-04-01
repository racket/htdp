;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname firrest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(define flat (list 1 2 3 4 5 6 7 8))

(define pair-list
  (list (list 10 11 12 13)
        (list 20 21 22 23)
        (list 30 31 32 33)
        (list 40 41 42 43)))

(define triple-list
  (list
   (list
    (list "alpha" "beta" "gamma" "delta")
    (list "epsilon" "zeta" "eta" "theta")
    (list "iota" "kappa" "lambda" "mu")
    (list "nu" "xi" "omicron" "pi"))
   (list
    (list "rho" "sigma" "tau" "upsilon")
    (list "phi" "chi" "psi" "omega")
    (list "eta2" "theta2" "iota2" "kappa2")
    (list "lambda2" "mu2" "nu2" "xi2"))
   (list
    (list "omicron2" "pi2" "rho2" "sigma2")
    (list "tau2" "upsilon2" "phi2" "chi2")
    (list "psi2" "omega2" "aleph" "beth")
    (list "gimel" "daleth" "he" "waw"))
   (list
    (list "zayin" "heth" "teth" "yodh")
    (list "kaph" "lamed" "mem" "nun")
    (list "samekh" "ayin" "pe" "tsade")
    (list "qoph" "resh" "shin" "taw"))))

(check-expect (firrest flat) 2)
(check-expect (firrerest flat) 3)
(check-expect (firrererest flat) 4)
(check-expect (rerest flat) (list 3 4 5 6 7 8))
(check-expect (rerererest (list 1 2 3 4)) empty)

(check-expect (firfirst pair-list) 10)
(check-expect (refirst pair-list) (list 11 12 13))
(check-expect (firrest pair-list) (list 20 21 22 23))
(check-expect (refirrest pair-list) (list 21 22 23))

(check-expect (firfirfirst triple-list) "alpha")
(check-expect (firfirrest triple-list) (list "rho" "sigma" "tau" "upsilon"))
(check-expect (firrefirst triple-list) (list "epsilon" "zeta" "eta" "theta"))
(check-expect (refirfirst triple-list) (list "beta" "gamma" "delta"))
(check-expect (firfirfirrest triple-list) "rho")

(check-expect (second flat) (firrest flat))
(check-expect (third flat) (firrerest flat))
(check-expect (fourth flat) (firrererest flat))
(check-expect (fifth flat) 5)
(check-expect (sixth flat) 6)
(check-expect (seventh flat) 7)
(check-expect (eighth flat) 8)
