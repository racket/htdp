(require-library "error.ss" "htdp")

(define-signature guiS
  (create-window show-window hide-window
   make-text make-choice make-button make-message
   text-contents choice-index draw-message
   ))
  
(define guessU
  (unit/sig guiS (import errorS plt:userspace^)

    ;; DEFAULT VALUES:
    ;; ----------------------------------------------------------------------------

    ;; the-frame : frame% 
    (define the-frame (make-object frame% "GUI" false 10 10))

    ;; the-panel : pane% ; for arranging stacks of GUI items 
    (define the-panel (make-object vertical-pane% the-frame))

    ;; INFRASTRUCTURE OPERATIONS:
    ;; ----------------------------------------------------------------------------

    ;; show-window : -> true
    ;; effect: to show the window
    (define (show-window)
      (send the-frame show true)
      true)

    ;; hide-window : -> true
    ;; effect: to hide the window 
    (define (hide-window . x)
      (send the-frame show false)
      true)

    ;; MAKING ITEMS: 
    ;; ----------------------------------------------------------------------------

    (define-struct gui-item (builder))
    ;; A gui-item[C < control%] is a structure: (make-gui-item f)
    ;; where f is a function: (panel% -> C)

    ;; create-gui-item : (panel% -> C[< control%])
    ;; to create a memoizing gui-item 
    (define (create-gui-item builder)
      (let ([C false])
	(make-gui-item (lambda (p) (or C (begin (set! C (builder p)) C))))))

    ;; create-window : (listof gui-item) -> true
    ;; to add gui-items to the window and to show window
    (define (create-window loi)
      (for-each (lambda (loi)
		  (let ((p (make-object horizontal-pane% the-panel)))
		    (send p set-alignment 'center 'center)
		    (for-each (lambda (i) ((gui-item-builder i) p)) loi)))
	loi)
      (show-window))
      
    ;; make-text : str -> gui-item
    ;; to create a text-item with label lbl
    (define (make-text lbl)
      (create-gui-item
	(lambda (the-panel) 
	  (make-object text-field% lbl the-panel void))))

    ;; make-message : str -> gui-item
    ;; to create a message-item with current contents txt
    (define (make-message txt)
      (create-gui-item 
	(lambda (the-panel)
	  (make-object message% txt the-panel))))

    ;; make-button : str (event% -> boolean) -> gui-item
    ;; to create a button-item with label and call-back function 
    (define (make-button label call-back)
      (create-gui-item 
	(lambda (the-panel)
	  (make-object button% label the-panel (lambda (b e) (call-back e))))))

    ;; make-choice : (listof str) -> gui-item
    ;; to create a choice-item that permits users to choose from the
    ;; alternatives on loc
    (define (make-choice loc)
      (create-gui-item 
	(lambda (the-panel)
	  (make-object choice% "" loc the-panel void))))

    ;; DISPLAYING MESSAGES: 
    ;; ----------------------------------------------------------------------------

    ;; draw-message : gui-item[message%] str -> true
    ;; to change the current contents of a message field 
    (define (draw-message msg txt)
      (send ((gui-item-builder msg) the-panel) set-label txt)
      true)

    ;; PROBING ITEMS: 
    ;; ----------------------------------------------------------------------------

    ;; text-contents : gui-item[text-field%] -> str
    ;; to determine the contents of a text-item 
    (define (text-contents a-text-gui)
      (send ((gui-item-builder a-text-gui) the-panel) get-value))

    ;; choice-index : gui-item[choice%] -> number
    ;; to determine which choice is currently selected in a choice-item 
    (define (choice-index a-choice)
      (send ((gui-item-builder a-choice) the-panel) get-selection))

    ))

(compound-unit/sig
  (import (PLT : plt:userspace^))
  (link
    (GUI  : guiS (guessU ERR PLT))
    (ERR  : errorS  (errorU)))
  (export (open GUI)))
