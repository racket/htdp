(module tool mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "tool.ss" "drscheme"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig ()
      (import drscheme:tool^)
      
      (define read/snips (lambda x (error x)))
      
      (define (snipize obj)
        (if (is-a? obj snip%)
            obj
            (make-string-snip obj)))
      
      (define (make-string-snip obj)
        (let* ([str (format "~a" obj)]
               [sn (make-object string-snip% (string-length str))])
          (send sn insert str (string-length str) 0)
          sn))
      
      (define void-snip%
        (class snip% 
          (inherit get-style)
          (define/override (copy)
            (let ([ans (make-object void-snip%)])
              (send ans set-style (get-style))
              ans))
          (super-instantiate ())))
      
      (define (make-delta family)
        (let ([d (make-object style-delta% 'change-family family)])
          (send d set-size-mult 0)
          (send d set-size-add (preferences:get 'drscheme:font-size))
          ;(send d set-delta-foreground "BLACK")
          d))
      
      (define renderable-editor-snip%
        (class editor-snip% 
          (init-field family color)
          (inherit get-editor get-style)
          
          [define pen (send the-pen-list find-or-create-pen color 1 'solid)]
          [define brush (send the-brush-list find-or-create-brush "BLACK" 'transparent)]
        
        (inherit get-extent get-inset)
        (rename [super-draw draw])
        (define/override draw
          (lambda (dc x y left top right bottom dx dy draw-caret)
            (let ([bl (box 0)]
                  [br (box 0)]
                  [bt (box 0)]
                  [bb (box 0)]
                  [bw (box 0)]
                  [bh (box 0)])
              (get-extent dc x y bw bh #f #f #f #f)
              (get-inset bl br bt bb)
              (super-draw dc x y left top right bottom dx dy draw-caret)
              (let ([old-pen (send dc get-pen)]
                    [old-brush (send dc get-brush)])
                (send dc set-pen pen)
                (send dc set-brush brush)
                (send dc draw-rectangle
                      (+ x (unbox bl))
                      (+ y (unbox bt))
                      (- (unbox bw) (unbox bl) (unbox br))
                      (- (unbox bh) (unbox bt) (unbox bb)))
                (send dc set-pen old-pen)
                (send dc set-brush old-brush)))))
        
        (define/override write
          (lambda (stream-out)
            (send (get-editor) write-to-file stream-out 0 'eof)))
        (define/override (copy)
          (let ([snip (make-snip)])
            (send snip set-editor (send (get-editor) copy-self))
            (send snip set-style (get-style))
            snip))
        (define/public (make-snip)
          (error 'make-snip "abstract method"))
        
        (define/public
          (make-editor)
          (make-object (drscheme:unit:program-editor-mixin plain-text%) (make-delta family)))
        
        (super-instantiate 
         ()
         (editor (make-editor))
         (with-border? #f))))
      
      (define constant-snip%
        (class* renderable-editor-snip% (drscheme:snip:special<%>) 
          (inherit get-editor)
          (inherit-field family)
          (define/public (read-special file line col pos)
            (with-syntax ([snip (make-object editor-snip% (send (get-editor) copy-self))]
                          [family family]
                          [replace-in-template replace-in-template]
                          [(snips ...)
                           (let loop ([snip (send (get-editor) find-first-snip)])
                             (cond
                               [(not snip) null]
                               [(transformable? snip)
                                (cons snip (loop (send snip next)))]
                               [else (loop (send snip next))]))])
              (values (syntax (replace-in-template 'family snip snips ...)) 1)))
          
          (define/public (get-family) family)
          
          (define/override (write stream-out)
            (send stream-out << (symbol->string family))
            (send (get-editor) write-to-file stream-out 0 'eof))
          (define/override (make-snip)
            (make-object constant-snip% family))
          
          (inherit show-border set-snipclass)
          (super-instantiate ()
            (color "BLUE"))
          (show-border #t)
          (set-snipclass constant-snipclass)))
      
      (define constant-snipclass%
        (class snip-class%
          (define/override (read stream-in)
            (let* ([family (string->symbol (send stream-in get-string))]
                   [snip (make-object constant-snip% (if (member family '(symbol roman modern))
                                                         family
                                                         'modern))])
              (send (send snip get-editor) read-from-file stream-in)
              snip))
          (super-instantiate ())))
      (define constant-snipclass (make-object constant-snipclass%))
      (send constant-snipclass set-version 1)
      (send constant-snipclass set-classname "robby:constant-snip")
      (send (get-the-snip-class-list) add constant-snipclass)
      
      (define evaluated-snip%
        (class* renderable-editor-snip% (drscheme:snip:special<%>)
          (inherit get-editor)
          
          (define/public (read-special file line col pos)
            (let ([text (get-editor)])
              (values
               (read-syntax
                text
                (gui-utils:read-snips/chars-from-text text 0 (send text last-position)))
               1)))
          
          
          ;; MATTHEW
          ;; cannot do this because the styles information in the saved texts screws up.
          (define/override (make-editor)
            (make-object (drscheme:unit:program-editor-mixin (scheme:text-mixin (editor:keymap-mixin text:basic%)))))
          
          (define/override (make-snip) (make-object evaluated-snip%))
          
          (inherit show-border set-snipclass)
          (super-instantiate () 
            (family 'modern)
            (color "RED"))
          (show-border #t)
          (set-snipclass evaluated-snipclass)))
      
      (define evaluated-snipclass%
        (class snip-class%
          (define/override (read stream-in)
            (let* ([snip (make-object evaluated-snip%)]
                   [editor (send snip get-editor)])
              (send editor read-from-file stream-in)
              snip))
          (super-instantiate ())))
      
      (define evaluated-snipclass (make-object evaluated-snipclass%))
      (send evaluated-snipclass set-version 1)
      (send evaluated-snipclass set-classname "robby:evaluated-snip")
      (send (get-the-snip-class-list) add evaluated-snipclass)
      
      (define plain-text%
        (class text:keymap% 
          (init-field [delta (make-object style-delta%)])
          (inherit change-style copy-self-to)
          (rename [super-after-insert after-insert]
                  [super-on-insert on-insert])
          (inherit begin-edit-sequence end-edit-sequence)
          (define/override (copy-self)
            (let ([t (make-object plain-text% delta)])
              (copy-self-to t)
              t))
          (define/override (on-insert x y)
            (super-on-insert x y)
            (begin-edit-sequence))
          (define/override (after-insert x y)
            (super-after-insert x y)
            (change-style delta x (+ x y))
            (end-edit-sequence))
          (inherit set-styles-sticky)
          (super-instantiate ())
          (set-styles-sticky #f)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                                     ;;;
      ;;;                      EVALUATION                     ;;;
      ;;;                                                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define (transformable? snip)
        (or (is-a? snip constant-snip%)
            (is-a? snip evaluated-snip%)))
      
      (define typeset-size
        (let ([value (preferences:get 'drscheme:font-size)])
          (case-lambda
            [() value]
            [(x)
             (unless (and (exact? x)
                          (integer? x)
                          (> x 0))
               (error 'typeset-size
                      "expected an exact integer strictly greater than zero"))
             (set! value x)])))
      
      (define (replace-in-template family template-snip . replacements)
        (let* ([delta (make-delta family)]
               [_ (begin (send delta set-delta-foreground "BLACK")
                         (send delta set-size-mult 0)
                         (send delta set-size-add (typeset-size)))]
               [text (make-object plain-text% delta)])
          (let loop ([replacements replacements]
                     [snip (send (send template-snip get-editor) find-first-snip)])
            (cond
              [(not snip)
               (unless (null? replacements)
                 (error 'replace-in-template "found end without doing all replacements: ~s" replacements))
               (void)]
              
              [(transformable? snip)
               (when (null? replacements)
                 (error 'replace-in-template "found replacable without replacement"))
               (let ([replacement (car replacements)]
                     [pos (send text get-snip-position snip)])
                 (send text insert (if (is-a? replacement snip%)
                                       (send replacement copy)
                                       (make-string-snip replacement))
                       (send text last-position) (send text last-position))
                 (loop (cdr replacements)
                       (send snip next)))]
              
              [else
               (send text insert (send snip copy) (send text last-position) (send text last-position))
               (loop replacements (send snip next))]))
          
          (let ([snip (make-object editor-snip% text #f
                        0 0 0 0
                        0 0 0 0)])
            (send text hide-caret #t)
            (send snip set-tight-text-fit #t)
            (send snip set-align-top-line #t)
            snip)))
      
      (define (typeset-frame-extension super%)
        (class super%
          (inherit get-editor get-menu-bar get-edit-target-object)
          
          (super-instantiate ())
          
          (let* ([mb (get-menu-bar)]
                 [menu (make-object menu% "Typeset" mb)]
                 [insert-snip
                  (lambda (make-obj)
                    (let ([editor (get-edit-target-object)])
                      (when editor
                        (let loop ([editor editor])
                          (let ([focused (send editor get-focus-snip)])
                            (if (and focused
                                     (is-a? focused editor-snip%))
                                (loop (send focused get-editor))
                                (let ([snip (make-obj)])
                                  (send editor insert snip)
                                  (send editor set-caret-owner snip 'display))))))))])
            (make-object menu:can-restore-menu-item% "Modern Constant Snip" menu
              (lambda (menu evt)
                (insert-snip
                 (lambda () (make-object constant-snip% 'modern))))
              #\m)
            (make-object menu:can-restore-menu-item% "Roman Constant Snip" menu
              (lambda (menu evt)
                (insert-snip 
                 (lambda () (make-object constant-snip% 'roman))))
              #\r)
            (make-object menu:can-restore-menu-item% "Symbol Constant Snip" menu
              (lambda (menu evt)
                (insert-snip 
                 (lambda () (make-object constant-snip% 'symbol))))
              #\r)
            (make-object menu:can-restore-menu-item% "Evaluated Snip" menu
              (lambda (menu evt)
                (insert-snip 
                 (lambda () (make-object evaluated-snip%))))))
          
          (frame:reorder-menus this)))
      
      (define (typeset-rep-extension super-text%)
        (class super-text%
          (rename [super-reset-console reset-console])
          (inherit get-user-namespace)
          
          (define/override (reset-console)
            (super-reset-console)
            (parameterize ([current-namespace (get-user-namespace)])
              (namespace-variable-binding 'single-bracket 'single-bracket)
              (namespace-variable-binding 'double-bracket 'double-bracket)
              ))
          
          (super-instantiate ())))
      
      (drscheme:get/extend:extend-unit-frame typeset-frame-extension)
      (drscheme:get/extend:extend-interactions-text typeset-rep-extension))))