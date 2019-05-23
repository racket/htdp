#lang racket/gui

;; provide some simple GUIs for logging and throwing away logs 

(provide
 ;; class: (init label) [log (-> Format-string Any ... Void]
 logging-gui%
 dummy-gui%)

;; an editor for logging messages 
(define logging-gui%
  (class frame%
    (init (label "Universe"))
    (inherit show)
    (super-new [label label][width 500][height 300])
    (field [text (new text%)]
           [edit (new editor-canvas% [parent this] [editor text])])
    
    ;; add lines to the end of the text 
    (define/public (log fmt . x)
      (define str (apply format (string-append fmt "\n") x))
      (queue-callback 
       (lambda () 
         (send text lock #f)
         (send text insert str (send text last-position))
         (send text lock #t))))
    
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
(require string-constants)

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
  (void))