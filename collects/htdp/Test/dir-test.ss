(define PLT (getenv "PLTHOME"))
(current-directory PLT)
(current-directory "lib")
(current-directory "teach")
(printf "in: ~a~n" (current-directory))
(define D (create-dir "."))

(length (dir-files D))
(length (dir-dirs D))
