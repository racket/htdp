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
      
      (define xml-box-color "green")
      (define scheme-splice-box-color "blue")
      (define scheme-box-color "purple")

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
          
          (define/override (make-editor) (make-object xml-text%))

          (define/public (read-special file line col pos)
            (let ([editor (get-editor)]
                  [old-locked #f])
              (dynamic-wind
               (lambda () 
                 (set! old-locked (send editor is-locked?))
                 (send editor lock #t))
               (lambda ()
                 (let* ([fill-chars (make-fill-chars editor)]
                        [port (make-custom-input-port #f fill-chars #f void)]
                        [xml (read-xml port)]
                        [xexpr (xml->xexpr (document-element xml))]
                        [expd-xexpr (expand-embedded xexpr)]
                        [qq-body (datum->syntax-object #'here expd-xexpr (list editor #f #f #f #f))])
                   (values
                    (with-syntax ([qq-body qq-body])
                      (syntax (quasiquote qq-body)))
                    1)))
               (lambda () (send editor lock old-locked)))))
          
          (define/override (write stream-out)
            (send (get-editor) write-to-file stream-out 0 'eof))
          (define/override (make-snip)
            (make-object xml-snip%))
          
          (inherit show-border set-snipclass)
          (super-instantiate ()
            (color xml-box-color))
          (show-border #t)
          (set-snipclass xml-snipclass)))

      ;; wrapped = (make-wraped sexp text number number number)
      (define-struct wrapped (snip text line col pos))
      
      ;; make-fill-chars : text -> string -> (union (tst number number number -> (values snip number)) number)
      ;; given an editor, makes the second argument to `make-custom-port'
      ;; that reads from the editor. If it finds a transformable?
      ;; snip, it returns snip via the ``special'' functionality of custom ports.
      (define (make-fill-chars text)
        (let ([ptr 0]
              [sema (make-semaphore 1)])
          (lambda (str)
            (semaphore-wait sema)
            (let ([snip (send text find-snip ptr 'after-or-none)])
              (begin0
                (cond
                  [(not snip)
                   eof]
                  [(transformable? snip)
                   (set! ptr (+ ptr 1))
                   (lambda (src line col pos)
                     (values (make-wrapped snip text line col pos) 1))]
                  [else
                   (string-set! str 0 (send text get-character ptr))
                   (set! ptr (+ ptr 1))
                   1])
                (semaphore-post sema))))))
              
      ;; expand-embedded : xexpr -> xexpr
      ;; constructs a new xexpr that has the embedded snips expanded 
      ;; and wrapped with unquotes
      (define (expand-embedded _xexpr)
        (let loop ([xexpr _xexpr])
          (cond
            [(pair? xexpr)
             (list* (car xexpr)
                    (cadr xexpr)
                    (map loop (cddr xexpr)))]
            [(wrapped? xexpr)
             (let* ([snip (wrapped-snip xexpr)]
                    [text (wrapped-text xexpr)]
                    [pos (wrapped-pos xexpr)]
                    [line (wrapped-line xexpr)]
                    [col (wrapped-col xexpr)])
               (let-values ([(stx wid) (send snip read-special text line col pos)])
                 (with-syntax ([stx stx])
                   (if (is-a? snip wrapped-snip%)
                       (syntax ,@stx)
                       (syntax ,stx)))))]
            [else xexpr])))
      
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

      (define scheme-snip%
        (class* renderable-editor-snip% (drscheme:snip:special<%>)
          (init-field splice?)
          (define/public (get-splice?) splice?)
          
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
          
          (define/override (make-snip) (make-object scheme-snip%))
          
          (inherit show-border set-snipclass)
          (super-instantiate () 
            (color (if splice?
                       scheme-box-color
                       scheme-box-color)))
          (show-border #t)
          (set-snipclass scheme-snipclass)))
      
      (define scheme-snipclass%
        (class snip-class%
          (define/override (read stream-in)
            (let* ([snip (make-object scheme-snip%)]
                   [editor (send snip get-editor)])
              (send editor read-from-file stream-in)
              snip))
          (super-instantiate ())))
      
      (define scheme-snipclass (make-object scheme-snipclass%))
      (send scheme-snipclass set-version 1)
      (send scheme-snipclass set-classname "drscheme:scheme-snip")
      (send (get-the-snip-class-list) add scheme-snipclass)
      
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
      
      (define xml-keymap (make-object keymap%))
      (send xml-keymap add-function "matching-xml" (lambda (x e) 
                                                 (when (is-a? x text%)
                                                   (matching-xml x))))
      (send xml-keymap map-function ">" "matching-xml")

      (define xml-keymap-mixin
        (mixin (editor:keymap<%>) ()
          (rename [super-get-keymaps get-keymaps])
          (define/override (get-keymaps)
            (cons xml-keymap (super-get-keymaps)))
          (super-instantiate ())))
      
      (define xml-text%
        (drscheme:unit:program-editor-mixin 
         (xml-keymap-mixin
          plain-text%)))

      ;; matching-xml : (is-a?/c text) -> void
      ;; inserts > and if there is an XML tag just
      ;; before the caret, inserts the corresponding
      ;; close XML tag after the caret.
      (define (matching-xml text)
        (send text begin-edit-sequence)
        (send text insert ">")
        (let* ([start (send text get-start-position)]
               [tagname (find-tag text start)])
          (when tagname
            (send text insert "</")
            (send text insert tagname)
            (send text insert ">")
            (send text set-position start)))
        (send text end-edit-sequence))
      
      ;; find-tag : (is-a?/c text%) number? -> (union false? string?)
      ;; finds the name of the XML tag just before `start' in `text'.
      ;; returns the tag name, with no trailing space of > or anything like that.
      (define (find-tag text start)
        ;; loop iterates backwards, searching for #\<
        ;; when it finds it, it gets the string starting
        ;; there, forwards to last-space (if there was a space)
        ;; or start-1
        ;; this technique gleaned from the spec at:
        ;;  http://www.w3.org/TR/2000/REC-xml-20001006
        (let loop ([pos (- start 2)]
                   [last-space #f])
          (cond
            [(< pos 0) #f]
            [else
             (let ([char (send text get-character pos)])
               (case char
                 [(#\>) #f]
                 [(#\<) (send text get-text (+ pos 1) (or last-space (- start 1)))]
                 [(#\space #\return #\newline #\tab)
                  (loop (- pos 1) pos)]
                 [else (loop (- pos 1) last-space)]))])))
      
      

                                                                      
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
            (is-a? snip scheme-snip%)))
      
      (define (xml-box-frame-extension super%)
        (class super%
          (inherit get-editor get-special-menu get-edit-target-object)
          
          (super-instantiate ())
          
          (let* ([menu (get-special-menu)]
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
                 (lambda () (make-object xml-snip%)))))
            (make-object menu:can-restore-menu-item% (string-constant xml-tool-scheme-box) menu
              (lambda (menu evt)
                (insert-snip 
                 (lambda () (instantiate scheme-snip% () (splice? #f))))))
            (make-object menu:can-restore-menu-item% (string-constant xml-tool-scheme-splice-box) menu
              (lambda (menu evt)
                (insert-snip
                 (lambda () (instantiate scheme-snip% () (splice? #t)))))))
          
          (frame:reorder-menus this)))
      
      (drscheme:get/extend:extend-unit-frame xml-box-frame-extension))))