(module gui mzscheme
  (require (lib "error.ss" "htdp")
	   (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (provide create-window show-window hide-window
	   make-text make-choice make-button make-message
	   text-contents choice-index draw-message)
  
    ;; DEFAULT VALUES:
    ;; ----------------------------------------------------------------------------

    ;; the-frame : frame% 
    (define the-frame (make-object frame% "GUI" false 10 10))

    ;; the-panel : pane% ; for arranging stacks of GUI items 
    (define the-panel (make-object vertical-panel% the-frame))

    ;; INFRASTRUCTURE OPERATIONS:
    ;; ----------------------------------------------------------------------------

    ;; show-window : -> true
    ;; effect: to show the window
    (define (show-window)
      (send the-frame show true)
      true)

    ;; hide-window : X -> true
    ;; effect: to hide the window 
    (define (hide-window x)
      (send the-frame show false)
      true)

    ;; MAKING ITEMS: 
    ;; ----------------------------------------------------------------------------

    (define-struct gui-item (builder))
    ;; A gui-item[C < control%] is a structure: (make-gui-item f)
    ;; where f is a function: (panel% -> C)

    ;; create-gui-item : ((union panel% #f) -> C[< control%])
    ;; to create a memoizing gui-item
    ;; create-window is the only caller that can pass in a panel
    ;; if the gui-item is already "panel'ed", raise an error signal
    ;; all other callers must pass in #f
    (define (create-gui-item builder)
      (let ([C false])
	(make-gui-item
	  (lambda (p)
	    (cond
	      [(and p C)
	       (error 'create-window "item added to window twice")]
	      [(and p (not C)) (set! C (builder p)) C]
	      [(and (not p) C) C]
	      [else (error 'internal "can't happen")])))))

    ;; create-window : (listof gui-item) -> true
    ;; to add gui-items to the window and to show window
    (define (create-window loi)
      (check-arg 'create-window
	(and (list? loi)
	     (andmap list? loi)
	     (andmap (lambda (l) (andmap gui-item? l)) loi))
	"list of lists of gui-items" "first" loi)
      (if up
	  (error 'create-window "only one window can be created at a time")
	  (set! up true))
      (for-each (lambda (loi)
		  (let ((p (make-object horizontal-pane% the-panel)))
		    (send p set-alignment 'center 'center)
		    (for-each (lambda (i) ((gui-item-builder i) p)) loi)))
	loi)
      (show-window))

    ;; up: boolean
    ;; to record whether the window is created 
    (define up false)

    ;; make-text : str -> gui-item
    ;; to create a text-item with label lbl
    (define (make-text lbl)
      (check-arg 'make-text (string? lbl) "string" "first" lbl)
      (create-gui-item
	(lambda (the-panel) 
	  (make-object text-field% lbl the-panel void))))

    ;; make-message : str -> gui-item
    ;; to create a message-item with current contents txt
    (define (make-message txt)
      (check-arg 'make-message (string? txt) "string" "first" txt)
      (create-gui-item 
	(lambda (the-panel)
	  (make-object message% txt the-panel))))

    ;; make-button : str (event% -> boolean) -> gui-item
    ;; to create a button-item with label and call-back function 
    (define (make-button label call-back)
      (check-arg 'make-button (string? label) "string" 'first label)
      (check-proc 'make-button call-back 1 'second "1 argument")
      (create-gui-item 
	(lambda (the-panel)
	  (make-object button% label the-panel (lambda (b e) (call-back e))))))

    ;; make-choice : (listof str) -> gui-item
    ;; to create a choice-item that permits users to choose from the
    ;; alternatives on loc
    (define (make-choice loc)
      (check-arg 'make-choice (and (list? loc) (andmap string? loc)) "list of strings" "first" loc)
      (create-gui-item 
	(lambda (the-panel)
	  (make-object choice% "" loc the-panel void))))

    ;; DISPLAYING MESSAGES: 
    ;; ----------------------------------------------------------------------------

    ;; draw-message : gui-item[message%] str -> true
    ;; to change the current contents of a message field 
    (define (draw-message msg txt)
      (check-arg 'draw-message (gui-item? msg) "gui-item" "first" msg)
      (check-arg 'draw-message (string? txt) "string" "second" txt)      
      (send ((gui-item-builder msg) #f) set-label txt)
      true)

    ;; PROBING ITEMS: 
    ;; ----------------------------------------------------------------------------

    ;; text-contents : gui-item[text-field%] -> str
    ;; to determine the contents of a text-item 
    (define (text-contents a-text-gui)
      (check-arg 'text-contents (gui-item? a-text-gui) "gui-item" "first" a-text-gui)
      (send ((gui-item-builder a-text-gui) #f) get-value))

    ;; choice-index : gui-item[choice%] -> number
    ;; to determine which choice is currently selected in a choice-item 
    (define (choice-index a-choice)
      (check-arg 'choice-index (gui-item? a-choice) "gui-item" "first" a-choice)      
      (send ((gui-item-builder a-choice) #f) get-selection))

    )
