#lang racket/base

(provide
 ;; map the directory tree at the given path into a data representation according to model 3 of 
 ;; HtDP/1e (part III) and HtDP/2e (Part IV); 
 ;; effects: if a directory d isn't accessible, the function prints (inaccessible d) to stdout
 create-dir ; String -> Dir.v3
 
 ; structure 
 dir? make-dir dir-name dir-dirs dir-files
 
 ; structure 
 file? [rename-out [create-file make-file]] file-name file-content file-size file-date

 make-date date-year date-month date-day date-hours date-minutes date-seconds)

;; ---------------------------------------------------------------------------------------------------

(require
  (only-in racket match-define natural-number/c)
  ; (only-in lang/htdp-beginner define-struct)
  htdp/error
  lang/prim
  (only-in racket/base
           [file-or-directory-modify-seconds s:file-or-directory-modify-seconds]
           [file-size s:file-size]))

;; Structures: 
(define-struct dir (name dirs files) #:transparent)
(define-struct file (name size date content) #:transparent)
(define-struct date (year month day hours minutes seconds) #:transparent)

(set! make-dir
      (let ([old make-dir])
        (lambda x
          (cond
            [(null? x) (error 'make-dir "expects 3 arguments, but found none" x)]
            [(null? (cdr x)) (error 'make-dir "expects 3 arguments, but found only 1")]
            [(null? (cddr x)) (error 'make-dir "expects 3 arguments, but found only 2")]
            [(> (length x) 3) (error 'make-dir "expects 3 arguments, but found ~a" (length x))]
            [else (apply old x)]))))

;; FilePath -> Date 
(define (create-date x)
  (define s (s:file-or-directory-modify-seconds x))
  (match-define (date* sc mi h d mo y wd yd dst z nano zname) (seconds->date s))
  (make-date y mo d h mi sc))

(define create-file
  (let* ([old make-file]
         [make-file 
          (case-lambda
            [(name size time content)
             (check-arg 'make-file (or (string? name) (symbol? name)) "string or symbol" "first" name)
             (check-arg 'make-file (natural-number/c size) "natural number" "second" size)
             (check-arg 'make-file (or (date? time) (and (number? time) (= 0 time))) "date (or 0)"
                        "third" time)
             (old name size time content)]
            [(name size content) (create-file name size 0 content)]
            [x (error 'make-file "expects 3 or 4 arguments, but found only ~a" (length x))])])
    make-file))

(define-primitive create-dir create-dir/proc)

;; Data:
;; Directory  = (make-dir Symbol (listof Dir) (listof File))
;; File       = (make-file Symbol Number Nat (union '() X))

(define (create-dir/proc a-path)
  (check-arg 'create-dir (string? a-path) "string" "first" a-path)
  (let ([a-path! (string->path a-path)])
    (if (directory-exists? a-path!)
        (car (explore (list a-path!)))
        (error 'create-dir "not a directory: ~e" a-path))))

;; explore : (listof String[directory-names]) -> (listof Directory)
(define (explore dirs)
  (map (lambda (d) 
         (let-values ([(fs ds) (pushd d directory-files&directories)]) 
           (define files (map (lambda (x) (build-path d x)) fs))
           (make-dir
            (string->symbol (path->string (my-split-path d)))
            (explore (map (lambda (x) (build-path d x)) ds))
            (map make-file
                 (map path->string fs)
                 (map (lambda (x) (if (file-exists? x) (s:file-size x) 0)) files)
                 (map (lambda (x) (if (file-exists? x) (create-date x) 0)) files)
                 (map (lambda (x) "") fs)))))
       dirs))

;; String -> String
(define (my-split-path d)
  (let-values ([(base name mbd?) (split-path d)])
    (if (string? base) name d)))

;; pushd : String[directory-name] (-> X) -> X
(define (pushd d f)
  (parameterize ([current-directory d])
    (f)))

;; directory-files&directories : 
;;  (-> (values (listof String[file-names]) (listof String[directory-names])))
(define (directory-files&directories)
  (with-handlers ((exn:fail:filesystem? 
                   (lambda (x)
                     (displayln `(inaccessible ,(current-directory)))
                     (values '() '()))))
    (let ((contents (directory-list)))
      (values
       (filter (lambda (x) (or (file-exists? x) (link-exists? x))) contents)
       (filter (lambda (x) (and (directory-exists? x) (not (link-exists? x))))
               contents)))))

;; get-file-content : file -> (int -> string)
;; to read a file on demand as a string
;; option to expand the library ... 
;; cache it ... 
(define (get-file-content f)
  (read-string (file-size f) (open-input-file (symbol->string (file-name f)))))
