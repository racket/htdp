;; TeachPack: dir.ss
;; Language: Beginner 

(define PLT (getenv "PLTHOME"))
(define current (create-dir "."))
(define teachps (create-dir (string-append PLT "/teachpack/htdp")))

(define current-files (map file-name (dir-files current)))
(define teachps-files (map file-name (dir-files teachps)))

(append
 (map (lambda (x) (format "in Teachpacks, not in Test: ~s" x))
      (filter (lambda (x) (not (member x current-files))) teachps-files))
 (map (lambda (x) (format "in Test, not in  Teachpacks: ~s" x))
      (filter (lambda (x) (not (member x teachps-files))) current-files)))

