;; TeachPack: dir.ss
;; Language: Beginner 

(define PLT (getenv "PLTHOME"))
(current-directory PLT)
(current-directory "teachpack")
(current-directory "htdp")
(printf "in: ~a~n" (current-directory))
(define D (create-dir "."))

(length (dir-files D))
(length (dir-dirs D))
