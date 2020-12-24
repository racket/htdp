;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require htdp/dir)

(define current (create-dir "."))
(define teachps (create-dir "/Users/matthias/plt/extra-pkgs/htdp/htdp-test/htdp/tests"))

(define current-files (map file-name (dir-files current)))
(define teachps-files (map file-name (dir-files teachps)))

(append
 (map (lambda (x) (format "in Teachpacks, not in Test: ~s" x))
      (filter (lambda (x) (not (member x current-files))) teachps-files))
 (map (lambda (x) (format "in Test, not in  Teachpacks: ~s" x))
      (filter (lambda (x) (not (member x teachps-files))) current-files)))

(check-expect (make-file "a" 1 2) (make-file "a" 1 2))
(check-expect (make-file 'a 1 2) (make-file 'a 1 2))

;; added to accommodate time field 
(check-expect (make-file "a" 1 0 4) (make-file "a" 1 0 4))

(define d (make-date 1 2 3 4 5 6))

(check-expect (make-file "b" 1 d 4) (make-file "b" 1 d 4))
(check-expect (date-year d) 1)
(check-expect (date-seconds d) 6)
(check-expect (date-minutes d) 5)
(check-expect (date-hours d) 4)
(check-expect (date-day d) 3)
(check-expect (date-month d) 2)

(check-error (make-file 1 2) "make-file: expects 3 or 4 arguments, but found only 2")
(check-error (make-file 1 2 3 4 5) "make-file: expects 3 or 4 arguments, but found only 5")
(check-error (make-file "a" -1 2) "make-file: expects a natural number as second argument, given -1")
(check-error (make-file "a" 1 2 3) "make-file: expects a date (or 0) as third argument, given 2")

(check-error (make-dir 1 2) "make-dir: expects 3 arguments, but found only 2")
(check-error (make-dir 1 2 3 4) "make-dir: expects 3 arguments, but found 4")
(check-error (make-dir 1 2 3) "make-dir: expects a string or symbol as first argument, given 1")
(check-error (make-dir 'a 2 3) "make-dir: expects a list of dir-s as second argument, given 2")
(check-error (make-dir "a" 2 3) "make-dir: expects a list of dir-s as second argument, given 2")
(check-error (make-dir "a" '[] 3) "make-dir: expects a list of file-s as third argument, given 3")
(check-error (make-dir "a" `[] 3) "make-dir: expects a list of file-s as third argument, given 3")
(check-error (make-dir "a" `[,current] 3) "make-dir: expects a list of file-s as third argument, given 3")

#| ---------------------------------------------------------------------------------------------------

RELEVANT EXCERPT FROM HTDP/2e

Model 3 Like directories, files have attributes. To introduce these, we proceed just as above.
First, we define a structure for files:

  (define-struct file [name size content])

Second, we provide a data definition:
; A File.v3 is a structure: 
;   (make-file String N String)
As indicated by the field names, the string represents the name of the file,
the natural number its size, and the string its content.

Finally, let’s split the content field of directories into two pieces: a list
of files and a list of sub-directories. This change requires a revision of the
structure type definition:

  (define-struct dir.v3 [name dirs files])

Here is the refined data definition:
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)
|#

(require "dir-aux.rkt")

(check-expect (dir-name teachps) "tests")

#; {Any -> Boolean : Dir.v3}
(define (dir.v3? x)
  (and (or (dir? x) (render 'dir1 x))
       (render 'dir2 (dir-name x))
       (andmap dir.v3? (dir-dirs x))
       (andmap file.v3? (dir-files x))))

#; {Any -> Boolean : File.v3}
(define (file.v3? x)
  (and (or (file? x) (render 'file1 x))
       (render 'file2 (file-name x))
       (natural-number/c (file-size x))
       (string? (file-content x))))

#; {Symbol Any -> False}
(define (render tag x)
  ;; this is a klugde to make sure no "path" shows up in names
  (or (and (string? x) (not (regexp-match "/" x)))
      (local ((define _ (display `[,tag failed with ,x])))
        #false)))

(check-expect (dir.v3? current) #true)
(check-expect (dir.v3? teachps) #true)