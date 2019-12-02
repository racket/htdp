#lang racket/gui

;; provide some simple GUIs for logging and throwing away logs 

(provide
 ;; class: (init label) [log (-> Format-string Any ... Void]
 logging-gui%
 dummy-gui%)

(require mzlib/pconvert)
(require string-constants)

;; -----------------------------------------------------------------------------
;; an editor for logging messages 
(define logging-gui%
  (class frame%
    (init (label "Universe"))
    (inherit show)
    (super-new [label (or label "! should not show !")][width 500][height 300])
    (field [text (new text%)]
           [edit (new editor-canvas% [parent this] [editor text])])

    ;; [Listof String] -> Void
    ;; EFFECT add str to end of text editor with 
    (define/public (make-queue-callback lostr (clear #f))
      (queue-callback
       (lambda ()
         (send text lock #f)
         (when clear (send text erase))
         (for ((str lostr))
           (send text insert str (send text last-position))
           (send text insert "\n" (send text last-position)))
         (send text lock #t))))
    
    ;; add lines to the end of the text 
    (define/public (log fmt . x)
      (define y
        (for/list ((i x))
          (parameterize ([constructor-style-printing #t]
                         [booleans-as-true/false     #t]
                         [abbreviate-cons-as-list    #t])
            (pretty-format (print-convert i) #:mode 'write))))
      (define str (apply format fmt y))
      (make-queue-callback (list str)))
    
    ;; -------------------------------------------------------------------------
    ;; add menu, lock, and show 
    (copy-and-paste this)
    (send text lock #t)))

;; throw away all messages, for Oliver to run on R Pi 
(define dummy-gui%
  (class object%
    (super-new)
    (define/public (show x) (void))
    (define/public (log x) (void))))

;; -----------------------------------------------------------------------------
;; Frame Text -> Void
;; add menu bar to frame for copying all of the text 

(define (copy-and-paste frame)
  (define mb (new menu-bar% [parent frame]))
  (define edit (new menu%
                    [label (string-constant edit-menu-label)]
                    [parent mb]))
  (new menu-item%
       [label (string-constant copy-menu-item)]
       [parent edit]
       [shortcut #\c]
       [callback (lambda (m e)
                   (define t (send frame get-focus-object))
                   (when (is-a? t editor<%>)
                     (send t copy)))])
  (new menu-item%
       [label (string-constant select-all-menu-item)]
       [parent edit]
       [shortcut #\a]
       [callback (lambda (m e)
                   (define t (send frame get-focus-object))
                   (when (is-a? t text%)
                     (send t set-position 0 (send t last-position))))])

  ;; The following is a first, experimental stab at a filtering facility so
  ;; that students can selectively view a subset of the events. 
  (new menu-item%
       [label "Filter"]
       [parent edit]
       [shortcut #\f]
       [callback
        (lambda (m e)
          (define t (send frame get-focus-object))
          (when (and (is-a? t editor<%>) (is-a? t text%))
            (define text  (send t get-text))
            (define lines (port->lines (open-input-string text)))
            (define pickd (filter-events "receive" "new state" lines))
            (send frame make-queue-callback pickd #t)))])
  (void))

;; Regexp-pattern Regexp-pattern [Listof String] -> [Listof String]
;; filter out stretches of lines between a "line" matching start-pat to one matching end-pat (incl.)
(define (filter-events start-part end-pat lines)
  (letrec ([pick
            (lambda (start-pat end-pat lines)
              (cond
                [(empty? lines) '()]
                [else
                 (define fst (first lines))
                 (if (regexp-match start-pat fst)
                     (let-values ([(front end) (up-to end-pat (rest lines))])
                       (append (list* fst front) (pick start-pat end-pat (rest lines))))
                     (pick start-pat end-pat (rest lines)))]))]
           [up-to
            (lambda (pat lines)
              (cond
                [(empty? lines) '()]
                [else (define fst (first lines))
                      (define rst (rest lines))
                      (if (regexp-match pat fst)
                          (values (list fst) rst)
                          (let-values ([(front end)  (up-to pat rst)])
                            (values (cons fst front) end)))]))])
    (pick "receive" "new state" lines)))

;; -----------------------------------------------------------------------------
;; watch how things work, not real tests 
(module+ main
  (struct foo (bar) #:prefab)
  (define (a-foo . _) (foo 1))

  (define logging-gui (new logging-gui% [label "testing"]))

  (send logging-gui show #t)

  (send logging-gui log "receive short S-expression: ~a" (build-list 3 add1))
  (send logging-gui log "new state ~a" 'a)
  (send logging-gui log "long S-expression: ~a" (build-list 100 add1))
  (send logging-gui log "new state ~a" 'b)
  (send logging-gui log "long string: ~a" (make-string 200 #\a))
  (send logging-gui log "new state ~a" 'c)
  (send logging-gui log "a string in a list: ~a" (list (make-string 200 #\a)))
  (send logging-gui log "new state ~a" 'd)
  (send logging-gui log "a list of structs: ~a" (build-list 10 a-foo))
  (send logging-gui log "new state ~a" 'e)
  (send logging-gui log "receive a byte string: ~a" #"hello world, good bye world")
  (send logging-gui log "new state ~a" 'f))

;; Don't run the almost-test in DrDr:
(module test racket/base
  (printf "Run a demo as the `main` submodule\n"))
