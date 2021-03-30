; Display test results in a window or panel.
#lang racket/base
(provide test-display-results!)

(require racket/class
         racket/gui/base
         framework
         (only-in simple-tree-text-markup/data empty-markup? empty-markup)
         test-engine/markup-gui
         test-engine/test-markup
         test-engine/test-engine
         string-constants)

(define (test-display-results! display-rep display-event-space markup)
  (cond
    [(empty-markup? markup)
     (clear-test-display! display-rep display-event-space)]
    [(string? markup)
     (clear-test-display! display-rep display-event-space)
     (display markup)]
    [display-event-space
     (parameterize ([current-eventspace display-event-space])
       (queue-callback
        (lambda ()
          (if display-rep
              (send display-rep display-test-results
                    (lambda (src-editor)
                      (popup-test-display! markup src-editor #t)))
              ;; this happens when called from the stepper
              ;; poor man's substitute, links don't work
              (popup-test-display! markup #f #t)))))]
    [else
     (error "no connection to test display")]))

(define (clear-test-display! display-rep display-event-space)
  (when display-event-space
    (parameterize ([current-eventspace display-event-space])
      (queue-callback
       (lambda ()
         (if display-rep
             (send display-rep display-test-results
                   (lambda (src-editor)
                     (define current-tab (definitions-tab src-editor))
                     (cond
                       [(and current-tab (send current-tab get-test-window))
                        => (lambda (window)
                             (send window clear))])))
             (popup-test-display! (empty-markup) #f #f)))))))

(define (popup-test-display! markup src-editor show?)
  (let* ([current-tab (definitions-tab src-editor)]
         [drscheme-frame (definitions-frame src-editor)]
         [curr-win (and current-tab (send current-tab get-test-window))]
         [window (or curr-win (make-object test-window%))]
         [content (make-object (text:foreground-color-mixin
                                (editor:standard-style-list-mixin
                                 (text:basic-mixin
                                  (editor:basic-mixin text%)))))])
    ;; prevent the framed boxes from messing up the style
    (send content set-styles-sticky #f)
    (insert-markup markup content src-editor #f)
    (send content lock #t)
    (send window update-editor content)
    (when current-tab
      (send current-tab current-test-editor content)
      (unless curr-win
        (send current-tab current-test-window window)
        (send drscheme-frame register-test-window window)
        (send window update-switch
              (lambda () (send drscheme-frame dock-tests)))
        (send window update-disable
              (lambda () (send current-tab update-test-preference #f)))
        (send window update-closer
              (lambda()
                (send drscheme-frame deregister-test-window window)
                (send current-tab current-test-window #f)
                (send current-tab current-test-editor #f)))))
    (if (and drscheme-frame
             (preferences:get 'test-engine:test-window:docked?))
        (send drscheme-frame display-test-panel content)
        (send window show show?))))

(define (definitions-tab definitions-text)
  (and definitions-text (send definitions-text get-tab)))

(define (definitions-rep definitions-text)
  (cond
   ((definitions-tab definitions-text) =>
    (lambda (tab)
      (send tab get-ints)))
   (else #f)))

(define (definitions-frame definitions-text)
  (cond
   ((definitions-tab definitions-text) =>
    (lambda (tab)
      (send tab get-frame)))
   (else #f)))

(frame:setup-size-pref 'htdp:test-engine-window-size 400 350
                       #:position-preferences 'htdp:test-engine-window-position)

(define test-window%
  (class* (frame:size-pref-mixin frame:standard-menus%) ()

    (super-new
     [label (string-constant test-engine-window-title)]
     [size-preferences-key 'htdp:test-engine-window-size]
     [position-preferences-key 'htdp:test-engine-window-position])

    (define switch-func void)
    (define disable-func void)
    (define close-cleanup void)

    (inherit get-area-container)
    
    (define content
      (make-object canvas:color% (get-area-container) #f '(auto-vscroll)))

    (define button-panel
      (make-object horizontal-panel% (get-area-container)
                   '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))

    (define buttons
      (list (make-object button%
                         (string-constant close)
                         button-panel
                         (lambda (b c)
                           (when (eq? 'button (send c get-event-type))
                             (close-cleanup)
                             (send this show #f))))
            (make-object button%
                         (string-constant dock)
                         button-panel
                         (lambda (b c)
                           (when (eq? 'button (send c get-event-type))
                             (send this show #f)
                             (preferences:set 'test-engine:test-window:docked? #t)
                             (switch-func))))
            (make-object grow-box-spacer-pane% button-panel)))

    (define/override (edit-menu:between-select-all-and-find menu) (void))
    
    (define/public (update-editor e)
      (send content set-editor e))

    (define/public (clear)
      (cond
        ((send content get-editor)
         => (lambda (editor)
              (send editor lock #f)
              (send editor delete 0 (send editor get-end-position))
              (send editor lock #t)))))

    (define/public (update-switch thunk)
      (set! switch-func thunk))
    (define/public (update-closer thunk)
      (set! close-cleanup thunk))
    (define/public (update-disable thunk)
      (set! disable-func thunk))))

