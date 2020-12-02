#lang scheme/gui

(require htdp/error mzlib/pconvert)

(provide checked-cell%)

(define checked-cell<%>
  (interface ()
    ;; Symbol Any -> Boolean 
    ;; does the new state differ from the old? 
    ;; effect: if so only, set state
    set

    ;; -> Any (ok?)
    get))

(define checked-cell% 
  (class* object% (checked-cell<%>) 
    (init-field
     ;; X
     value0
     ;; Any -> Boolean : X 
     ok?)  

    ;; (U String #f) ; a string is the name of the state display window
    (init [display #f])
    
    (field 
     [value (coerce "the initial expression" value0 #t)]
     ;; (U False pasteboard%)
     [pb (cond
           [(boolean? display) #f]
           [else [define f (new frame% [label display][width 400][height 400])]
                 [define p (new pasteboard%)]
                 [define e (new editor-canvas% [parent f] [editor p])]
                 (send f show #t)
                 p])])
    
    ;; -> Void
    ;; EFFECT insert the current state into the pasteboard
    (define/private (show-state)
      (define xbox (box #f)) ;; x coordinate (throw away)
      (define ybox (box 0))  ;; y coordinate for next snip
      (define value:print
        (parameterize ([constructor-style-printing #t]
                       [booleans-as-true/false     #t]
                       [abbreviate-cons-as-list    #t])
          (print-convert value)))
      (define value:pretty-string (pretty-format #:mode 'write value:print 80))
      ;; turn s into lines and display them in pb
      (send pb erase)
      (if (is-a? value snip%)
          (send pb insert value 0 0)
          (parameterize ([current-input-port (open-input-string value:pretty-string)])
            (let read-all ()
              (define nxt (read-line))
              (unless (eof-object? nxt)
                (define snip (make-object string-snip% nxt))
                (send pb insert snip 0 (unbox ybox))
                (send pb get-snip-location snip xbox ybox #t)
                (read-all))))))

    ;; Symbol Any Boolean -> Any : ok?
    ;; ensure that nw is an ok? value; otherwise signal an error from tag, with say-evaluated-to
    (define/private (coerce tag nw [say-evaluated-to #f])
      (define bool-fmt "the test function ~a is expected to return a boolean, but it returned ~v")
      (define ok-fmt   "~a ~a ~v, which fails to pass check-with's ~a test")
      (define name   (symbol->string (object-name ok?)))
      (define ok?-nw (ok? nw))
      (unless (boolean? ok?-nw) (tp-error 'check-with bool-fmt name ok?-nw))
      (unless ok?-nw
        (define check-with-name (if (regexp-match "check-with" name) "handler" name))
        (define word (if say-evaluated-to "evaluated to" "returned"))
        (tp-error 'check-with ok-fmt tag word nw check-with-name))
      nw)
    
    ;; Symbol Any -> Void 
    ;; effect: set value to v if distinct, also display it if pb exists
    (define/public (set tag v) 
      (define nw  (coerce tag v))
      ;; this is the old Robby "optimization" for not triggering draw
      ;; when the world doesn't change 
      ;if (equal? value nw)
      ;   #t
      (begin
        (set! value nw)
        (when pb (show-state))
        #f))
    
    ;; -> ok?
    (define/public (get) value)
    
    (super-new)
    
    (when pb (show-state))))

; (define c (new checked-cell% [msg "World"] [value0 1] [ok? positive?]))
; (send c set "tick" 10)
