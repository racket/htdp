"
Assume and export: 
  (struct dir (name dirs files))
  (struct file (name size content))

A directory is 
  (make-dir symbol (listof directory) (listof file))
A file is 
  (make-file symbol)

Export: 
  create-dir : string[path-name] -> directory."

(reference-file "error-lib.ss")

(define-signature dirS
  ((struct dir (name dirs files))
   (struct file (name size content))
   create-dir))
  
(define dirU
  (let ((f-s (lambda (x) (if (link-exists? x) 0 (file-size x)))))
    (unit/sig dirS
      (import errorS plt:userspace^)

      (define-struct dir (name dirs files))
  
      (define-struct file (name size content))
  
      ;; create-dir : path -> directory
      (define (create-dir a-path)
	(check-arg 'create-dir (string? a-path) "string" "first" a-path)
	(if (directory-exists? a-path)
	    (car (explore (list a-path)))
	    (error 'create-dir "not a directory: ~e" a-path)))
  
      ;; explore : (listof directory-names) -> (listof directory)
      (define (explore dirs)
	(map (lambda (d) 
	       (let-values ([(fs ds) (pushd d directory-files&directories)]) 
		 (make-dir
		   (string->symbol (my-split-path d))
		   (explore (map (lambda (x) (build-path d x)) ds))
		   (map make-file
		     (map string->symbol fs)
		     (map f-s (map (lambda (x) (build-path d x)) fs))
		     (map (lambda (x) (if (link-exists? x) 'link null)) fs)))))
	  dirs))
  
      (define (my-split-path d)
	(let-values ([(base name mbd?) (split-path d)])
	  (if (string? base) name d)))
	       
      ;; pushd : directory-name ( () -> X ) -> X
      (define (pushd d f)
	(let ((current (current-directory)))
	  (dynamic-wind
	    (lambda () (current-directory d))
	    (lambda () (f))
	    (lambda () (current-directory current)))))
  
      ;; directory-files&directories : 
      ;;  () -> (values (listof file-names) (listof directory-names))
      (define (directory-files&directories)
	(let ((contents (directory-list)))
	  (values
	    (filter (lambda (x) (or (file-exists? x) (link-exists? x))) contents)
	    (filter (lambda (x)
		      (and (directory-exists? x) (not (link-exists? x))))
	            contents))))
  
      ;; filter: (X -> bool) (listof X) -> (listof X)
      ;;  (define (filter p? l)
      ;;    (foldr (lambda (fst rst) (if (p? fst) (cons fst rst) rst)) '() l))

  
      ;; get-file-content : file -> (int -> string)
      ;; to read a file on demand as a string
      ;; option to expand the library ... 
      ;; cache it ... 
      (define (get-file-content f)
        (read-string (file-size f) 
                     (open-input-file (symbol->string (file-name f)))))
      
      ; Test: 
      ; (define G (create-dir "."))
      ; (define foo (assf (lambda (x) (eq? 'dir-test.ss (file-name x))) (dir-files G)))
      ; (get-file-content foo)
      )))

(compound-unit/sig
  (import (PLT : plt:userspace^))
  (link
    (XXX : dirS (dirU ERR PLT))
    (ERR  : errorS (errorU)))
  (export (open XXX)))
