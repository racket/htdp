(module tool mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "tool.ss" "drscheme")
           (lib "xml.ss" "xml")
           (lib "string-constant.ss" "string-constants"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define xml-box-color "purple")
      (define scheme-box-color "blue")

      (define (make-string-snip obj)
        (let* ([str (format "~e" obj)]
               [sn (make-object string-snip% (string-length str))])
          (send sn insert str (string-length str) 0)
          sn))
      
      (define renderable-editor-snip%
        (class editor-snip% 
          (init-field color)
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
          
          (define/public (make-editor)
            (make-object (drscheme:unit:program-editor-mixin plain-text%)))
          
          (super-instantiate ()
            (editor (make-editor))
            (with-border? #f))))
      
      (define xml-snip%
        (class* renderable-editor-snip% (drscheme:snip:special<%>) 
          (inherit get-editor)
          (define/public (read-special file line col pos)
            (send (get-editor) lock #t)
            (let* ([fill-chars (make-fill-chars (get-editor))]
                   [raw-syntaxes (get-syntaxes (get-editor))]
                   [unquote-syntaxes (add-unquotes raw-syntaxes)]
                   [port (make-custom-input-port #f fill-chars #f void)]
                   [xml (read-xml port)]
                   [xexpr (xml->xexpr (document-element xml))]
                   [subd-xexpr (substitute xexpr unquote-syntaxes)]
                   [qq-body (datum->syntax-object #'here subd-xexpr)])
              (send (get-editor) lock #f)
              (values
               (with-syntax ([qq-body qq-body])
                 (syntax (quasiquote qq-body)))
               1)))
          
          (define/override (write stream-out)
            (send (get-editor) write-to-file stream-out 0 'eof))
          (define/override (make-snip)
            (make-object xml-snip%))
          
          (inherit show-border set-snipclass)
          (super-instantiate ()
            (color xml-box-color))
          (show-border #t)
          (set-snipclass xml-snipclass)))

      ;; funny-magic-string-constant and funny-magic-symbol-constant
      ;; have to match up.
      (define funny-magic-string-constant "&backdoor;")
      (define funny-magic-symbol-constant 'backdoor)
      
      ;; make-fill-chars : text -> string -> number
      ;; given an editor, makes the second argument to `make-custom-port'
      ;; that reads from the editor. If it finds a transformable?
      ;; snip, it returns the funny-magic-string-constant.
      (define op (current-output-port))
      (define (make-fill-chars text)
        (let ([ptr 0]
              [in-funny-string #f]
              [b (box 0)]
              [sema (make-semaphore 1)])
          (lambda (str)
            (semaphore-wait sema)
            (letrec ([snip (send text find-snip ptr 'after-or-none)]
                     [funny ;; pre: (number? in-funny-string)
                      (lambda ()
                        (cond
                          [(< in-funny-string (string-length funny-magic-string-constant))
                           (string-set! str 0 (string-ref funny-magic-string-constant in-funny-string))
                           (set! in-funny-string (+ in-funny-string 1))
                           1]
                          [else
                           (set! in-funny-string #f)
                           (regular)]))]
                     [regular ;; pre: (not in-funny-string)
                      (lambda ()
                        (cond
                          [(not snip)
                           eof]
                          [(transformable? snip)
                           (set! ptr (+ ptr 1))
                           (set! in-funny-string 0)
                           (funny)]
                          [else
                           (string-set! str 0 (send text get-character ptr))
                           (set! ptr (+ ptr 1))
                           1]))])
              (begin0
                (cond
                  [in-funny-string
                   (funny)]
                  [else
                   (regular)])
                (semaphore-post sema))))))
              
      ;; get-syntaxes : text -> (listof syntax)
      ;; extracts the syntax from each renderable snip in the editor
      (define (get-syntaxes text)
        (let loop ([snip (send text find-first-snip)])
          (cond
            [(not snip) null]
            [(transformable? snip)
             (let* ([pos (send text get-snip-position snip)]
                    [line (send text position-paragraph pos)]
                    [col (- pos (send text paragraph-start-position line))])
               (let-values ([(stx wid) (send snip read-special text line col pos)])
                 (cons stx (loop (send snip next)))))]
            [else (loop (send snip next))])))
      
      ;; add-unquotes : (listof syntax) -> (listof syntax)
      ;; adds an unquote to the front of each syntax object
      (define (add-unquotes syntaxes)
        (map (lambda (x)
               (with-syntax ([x x])
                 (syntax (unquote x))))
             syntaxes))
   
      ;; substitute : xexpr (listof syntax) -> xexpr
      ;; constructs a new xexpr that substitutes the funny symbol with the syntaxes
      (define (substitute _xexpr syntaxes)
        (let ([syntaxes syntaxes])
          (let loop ([xexpr _xexpr])
            (cond
              [(pair? xexpr)
               (list* (car xexpr)
                      (cadr xexpr)
                      (map loop (cddr xexpr)))]
              [(eq? funny-magic-symbol-constant xexpr)
               (when (null? syntaxes)
                 (error 'substitute "not enough funny symbols: ~e" _xexpr))
               (begin0
                 (car syntaxes)
                 (set! syntaxes (cdr syntaxes)))]
              [else xexpr]))))
      
      (define xml-snipclass%
        (class snip-class%
          (define/override (read stream-in)
            (let* ([snip (make-object xml-snip%)])
              (send (send snip get-editor) read-from-file stream-in)
              snip))
          (super-instantiate ())))
      (define xml-snipclass (make-object xml-snipclass%))
      (send xml-snipclass set-version 1)
      (send xml-snipclass set-classname "drscheme:xml-snip")
      (send (get-the-snip-class-list) add xml-snipclass)
      
      (define evaluated-snip%
        (class* renderable-editor-snip% (drscheme:snip:special<%>)
          (inherit get-editor)
          
          (define/public (read-special file line col pos)
            (let ([text (get-editor)])
              (values
               (read-syntax
                text
                (drscheme:language:open-input-text text 0 (send text last-position)))
               1)))
          
          
          ;; MATTHEW
          ;; cannot do this because the styles information in the saved texts screws up.
          (define/override (make-editor)
            (make-object (drscheme:unit:program-editor-mixin 
                          (scheme:text-mixin (editor:keymap-mixin text:basic%)))))
          
          (define/override (make-snip) (make-object evaluated-snip%))
          
          (inherit show-border set-snipclass)
          (super-instantiate () 
            (color scheme-box-color))
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
      

                                                                      
                      ;;;                           ;                 
                        ;                   ;                         
                        ;                   ;                         
  ;;;  ;;; ;;; ;;;;     ;   ;;  ;;  ;;;;   ;;;;;  ;;;     ;;;  ; ;;;  
 ;   ;  ;   ;      ;    ;    ;   ;      ;   ;       ;    ;   ;  ;;  ; 
 ;;;;;  ;   ;   ;;;;    ;    ;   ;   ;;;;   ;       ;    ;   ;  ;   ; 
 ;       ; ;   ;   ;    ;    ;   ;  ;   ;   ;       ;    ;   ;  ;   ; 
 ;   ;   ;;;   ;   ;    ;    ;   ;  ;   ;   ;   ;   ;    ;   ;  ;   ; 
  ;;;     ;     ;;; ; ;;;;;;  ;;; ;  ;;; ;   ;;;  ;;;;;   ;;;  ;;;  ;;
                                                                      
                                                                      
                                                                      
      
      (define (transformable? snip)
        (or (is-a? snip xml-snip%)
            (is-a? snip evaluated-snip%)))
      
      (define (xml-box-frame-extension super%)
        (class super%
          (inherit get-editor get-menu-bar get-edit-target-object)
          
          (super-instantiate ())
          
          (let* ([mb (get-menu-bar)]
                 [menu (make-object menu% (string-constant xml-tool-menu) mb)]
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
            (make-object menu:can-restore-menu-item% (string-constant xml-tool-xml-box) menu
              (lambda (menu evt)
                (insert-snip
                 (lambda () (make-object xml-snip%))))
              #\m)
            (make-object menu:can-restore-menu-item% (string-constant xml-tool-scheme-box) menu
              (lambda (menu evt)
                (insert-snip 
                 (lambda () (make-object evaluated-snip%))))
              #\r))
          
          (frame:reorder-menus this)))
      
      (drscheme:get/extend:extend-unit-frame xml-box-frame-extension))))
