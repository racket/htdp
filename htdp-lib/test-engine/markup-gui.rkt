; Insert markup into text% editors, making links clickable.
#lang racket/base
(require racket/contract)         
(provide (contract-out
          (insert-markup ((markup? (is-a?/c text%) (or/c #f (is-a?/c text%))) (boolean?) . ->* . any))))

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

; It's advisable to (send text set-styles-sticky #f) on text% editors
; that have editor:standard-style-list-mixin.
; Otherwise, framed boxes mess up the formatting.

; inline? means there might be stuff around the insert-markup, so if
; it's #t, we need to use a snip for vertical markup
(define (insert-markup markup text src-editor (inline? #t))
  (cond
    ((string? markup)
     (send text insert markup))
    ((empty-markup? markup) (void))
    ((horizontal-markup? markup)
     (for-each (lambda (markup)
                 (insert-markup markup text src-editor))
               (horizontal-markup-markups markup)))
    ((vertical-markup? markup)
     (define (insert-vertical text)
       (for-each/between
        (lambda (markup) (insert-markup markup text src-editor #f))
        (lambda () (send text insert #\newline))
        (vertical-markup-markups markup)))
     (if inline?
         (send text insert (make-snip insert-vertical))
         (insert-vertical text)))
    ((srcloc-markup? markup)
     (insert-srcloc-markup markup text src-editor))
    ((framed-markup? markup)
     (insert-framed (framed-markup-markup markup) text src-editor))
    ((image-markup? markup)
     (let ((data (image-markup-data markup)))
       (cond
         ((is-a? data snip%)
          (send text insert data))
         ((is-a? data bitmap%)         ;; works in other places, so include it here too
          (send text insert (make-object image-snip% data)))
         ((record-dc-datum? data)
          (cond
            ((record-dc-datum->bitmap data (record-dc-datum-width data) (record-dc-datum-height markup))
             => (lambda (bitmap)
                  (send text insert (make-object image-snip% bitmap))))
            (else
             (insert-markup (image-markup-alt-markup markup) text src-editor))))
         (else
          (insert-markup (image-markup-alt-markup markup) text src-editor)))))))
     
(define (for-each/between proc between list)
  (let loop ((list list))
    (cond
      ((null? list) (values))
      ((null? (cdr list))
       (proc (car list)))
      (else
       (proc (car list))
       (between)
       (loop (cdr list))))))

(define (make-snip insert)
  (let* ([text (new framed-text%)]
         [snip (new editor-snip% [editor text] [with-border? #f])])
    (send text set-styles-sticky #f)
    (send snip use-style-background #t)
    (insert text)
    (send text lock #t)
    snip))
         
(define (record-dc-datum->bitmap datum width height)
  (with-handlers ((exn:fail? (lambda (e) #f)))
    (let ((proc (recorded-datum->procedure datum))
          (bitmap (make-object bitmap% width height)))
      (let ((dc (new bitmap-dc% [bitmap bitmap])))
        (proc dc)
        bitmap))))          

(define framed-text%
  (text:foreground-color-mixin
   (text:wide-snip-mixin
    (text:basic-mixin
     (editor:standard-style-list-mixin
      (editor:basic-mixin
       text%))))))

(define (insert-framed markup text src-editor)
  (let* ([framed-text (new framed-text%)]
         [snip (new editor-snip% [editor framed-text])])
    (send text set-styles-sticky #f)
    (send snip use-style-background #t)
    (insert-markup markup framed-text src-editor #f)
    (when (color-prefs:known-color-scheme-name? 'drracket:read-eval-print-loop:value-color)
      (send framed-text change-style
            (color-prefs:lookup-in-color-scheme 'drracket:read-eval-print-loop:value-color)
            0 (send framed-text get-end-position)))
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
         (text (new (editor:standard-style-list-mixin
                     (text:basic-mixin
                      (editor:basic-mixin text%)))))
         (canvas (new editor-canvas% [parent frame])))
    (send text set-styles-sticky #t)
    (send canvas set-editor text)
    (insert-markup markup text text #f)
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
                "Framed via text")

  (check-equal? (render-markup-via-text (horizontal "foo" (vertical "bar" "baz") "bam"))
                "foobar\nbazbam"
                "Framed via text"))
