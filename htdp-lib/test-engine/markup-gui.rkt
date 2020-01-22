; Insert markup into text% editors, making links clickable.
#lang racket/base
(require racket/contract)         
(provide (contract-out
          (insert-markup (markup? (is-a?/c text%) (or/c #f (is-a?/c text%)) . -> . any))))

(require (only-in racket/class send make-object is-a? new is-a?/c)
         simple-tree-text-markup/data
         racket/gui/base
         racket/snip
         framework)

; src-editor can be #f
(define (insert-srcloc-markup srcloc-markup text src-editor)
  (let ((srcloc (srcloc-markup-srcloc srcloc-markup))
        (start (send text get-end-position)))
    (insert-markup (srcloc-markup-markup srcloc-markup) text src-editor)
    (when src-editor
      (send text set-clickback
            start (send text get-end-position)
            (lambda (t s e) (highlight-srcloc srcloc src-editor))
            #f #f)
      (let ([end (send text get-end-position)])
        (when (color-prefs:known-color-scheme-name? 'drracket:read-eval-print-loop:value-color)
          (send text change-style
                (color-prefs:lookup-in-color-scheme 'drracket:read-eval-print-loop:value-color)
                start end #f))
        (send text change-style
              (make-object style-delta% 'change-underline #t)
              start end #f)))))

(define (definitions-tab definitions-text)
  (and definitions-text (send definitions-text get-tab)))

(define (definitions-rep definitions-text)
  (cond
   ((definitions-tab definitions-text) =>
    (lambda (tab)
      (send tab get-ints)))
   (else #f)))

(define (highlight-srcloc srcloc src-editor)
  (let ((current-rep (definitions-rep src-editor)))
    (when (and current-rep src-editor
               (is-a? src-editor text:basic<%>))
      (send current-rep highlight-errors
            (list srcloc) #f)
      (let* ([current-tab (definitions-tab src-editor)]
             [frame (send current-tab get-frame)])
        (unless (send current-tab is-current-tab?)
          (let loop ([tabs (send frame get-tabs)] [i 0])
            (unless (null? tabs)
              (if (eq? (car tabs) current-tab)
                  (send frame change-to-nth-tab i)
                  (loop (cdr tabs) (add1 i))))))
        (send frame show #t)))))

; It's advisable to (send text set-styles-sticky #t) on text% editors
; that have editor:standard-style-list-mixin.
; Otherwise, framed boxes mess up the formatting.
(define (insert-markup markup text src-editor)
  (cond
    ((string? markup)
     (send text insert markup))
    ((empty-markup? markup) (void))
    ((horizontal-markup? markup)
     (for-each (lambda (markup)
                 (insert-markup markup text src-editor))
               (horizontal-markup-markups markup)))
    ((vertical-markup? markup)
     (for-each (lambda (markup)
                 (insert-markup markup text src-editor)
                 (send text insert #\newline))
               (vertical-markup-markups markup)))
    ((srcloc-markup? markup)
     (insert-srcloc-markup markup text src-editor))
    ((framed-markup? markup)
     (insert-framed (framed-markup-markup markup) text src-editor))))

(define framed-text%
  (text:wide-snip-mixin
   (text:basic-mixin
    (editor:standard-style-list-mixin
     (editor:basic-mixin
      text%)))))

(define (border-style-delta)
  (let ((delta (new style-delta%)))
    (send delta set-delta-foreground 
          (color-prefs:lookup-in-color-scheme 'framework:default-text-color))
    delta))

(define (insert-framed markup text src-editor)
  (let* ([framed-text (new framed-text%)]
         [snip (new editor-snip% [editor framed-text])])
    (send snip use-style-background #t)
    (when (color-prefs:known-color-scheme-name? 'drracket:read-eval-print-loop:value-color)
      (send framed-text change-style
            (color-prefs:lookup-in-color-scheme 'drracket:read-eval-print-loop:value-color)))
    (insert-markup markup framed-text src-editor)
    (send framed-text lock #t)

    (let ((before (send text get-end-position)))
      (send text insert snip)
      (when (color-prefs:known-color-scheme-name? 'drracket:read-eval-print-loop:out-color)
        (send text change-style
              (color-prefs:lookup-in-color-scheme 'drracket:read-eval-print-loop:out-color)
              before (send text get-end-position))))))

;; for development
(define (display-markup markup)
  (let* ((frame (new frame%
                     [label "Markup"]
                     [width 600] [height 400]))
         (text (new (text:foreground-color-mixin
                     (editor:standard-style-list-mixin
                      (text:basic-mixin
                       (editor:basic-mixin text%))))))
         (canvas (new editor-canvas% [parent frame])))
    (send text set-styles-sticky #f)
    (send canvas set-editor text)
    (insert-markup markup text text)
    (send text lock #t)
    (send frame show #t)))


(module+ test
  (require rackunit
           simple-tree-text-markup/construct)

  (define (render-markup-via-text markup)
    (let ((text (new text%)))
      (insert-markup markup text #f)
      (send text get-text 0 'eof #t)))

  (check-equal? (render-markup-via-text "foo")
                "foo"
                "String via text")
  (check-equal? (render-markup-via-text (horizontal "foo" (framed-markup "bar") "baz"))
                "foobarbaz"
                "Framed via text"))
