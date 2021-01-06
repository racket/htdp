#lang scheme

(provide tool@)

(require string-constants
         framework
         (prefix-in et: errortrace/stacktrace)
         mzlib/pretty
         (prefix-in pc: mzlib/pconvert)
         mzlib/file
         mzlib/unit
         mzlib/class
         mzlib/list
         mzlib/struct
         mzlib/struct
         drscheme/tool
         mred
         mrlib/cache-image-snip
         (prefix-in ic: mrlib/image-core)

         ;; this module is shared between the drracket namespace (so loaded here) 
         ;; and the user's namespace in the teaching languages
         "private/set-result.rkt"
         "private/rewrite-error-message.rkt"

         "private/continuation-mark-key.rkt"
         "private/create-htdp-executable.rkt"
         "private/tp-dialog.rkt"
         
         "stepper-language-interface.rkt"
         lang/debugger-language-interface
         "run-teaching-program.rkt"
         "htdp-langs-save-file-prefix.rkt"
         "htdp-langs-interface.rkt"

         (only-in test-engine/syntax
                  report-signature-violation! test-execute test)
	 (except-in test-engine/test-engine signature-violation)
         test-engine/test-markup
         test-engine/test-display-gui
         deinprogramm/signature/signature)

(require setup/collects
         setup/getinfo)

(define HTDP-TEACHPACK-MODULE-KEY '2htdp:teachpack-modules)
(define HTDP-TEACHPACK-INFO-KEYS
  (list HTDP-TEACHPACK-MODULE-KEY))

(define o (current-output-port))
(define (oprintf . args) (apply fprintf o args))
  
(define tool@
  (unit 
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
      
    (define drs-eventspace (current-eventspace))
      
    ;; tracing? : boolean
    ;; teachpacks : (listof require-spec)
    (define-struct (htdp-lang-settings drscheme:language:simple-settings)
      (tracing? teachpacks true/false/empty-as-ids?))
    (define htdp-lang-settings->vector (make-->vector htdp-lang-settings))
    (define teachpacks-field-index (+ (procedure-arity drscheme:language:simple-settings) 1))
      
    (define image-string "<image>")
      
    ;; module-based-language-extension :    (implements drscheme:language:module-based-language<%>) 
    ;;                                   -> (implements drscheme:language:module-based-language<%>)
    ;; changes the default settings and sets a few more paramters during `on-execute'
    (define (module-based-language-extension super%)
      (class* super% ()
          
        (inherit get-sharing-printing get-abbreviate-cons-as-list)
          
        (define/override (default-settings)
          (make-htdp-lang-settings 
           #t
           'constructor
           'repeating-decimal
           (get-sharing-printing)
           #t
           'none
           #f 
           (preferences:get 'drracket:htdp:last-set-teachpacks/multi-lib)
           #f))
          
        (define/override (default-settings? s)
          (and (super default-settings? s)
               (not (htdp-lang-settings-tracing? s))
               (null? (htdp-lang-settings-teachpacks s))
               (not (htdp-lang-settings-true/false/empty-as-ids? s))))
          
        (define/override (marshall-settings x)
          (list (super marshall-settings x)
                (htdp-lang-settings-tracing? x)
                (htdp-lang-settings-teachpacks x)
                (htdp-lang-settings-true/false/empty-as-ids? x)))
          
        (define/override (unmarshall-settings x)
          (cond
            [(and (list? x)
                  (or (= (length x) 3)
                      (= (length x) 4))
                  (boolean? (list-ref x 1))
                  (list-of-require-specs? (list-ref x 2))
                  (implies (= (length x) 4)
                           (boolean? (list-ref x 3))))
             (define drs-settings (super unmarshall-settings (first x)))
             (make-htdp-lang-settings
              (drscheme:language:simple-settings-case-sensitive drs-settings)
              (drscheme:language:simple-settings-printing-style drs-settings)
              (drscheme:language:simple-settings-fraction-style drs-settings)
              (drscheme:language:simple-settings-show-sharing drs-settings)
              (drscheme:language:simple-settings-insert-newlines drs-settings)
              (drscheme:language:simple-settings-annotations drs-settings)
              (cadr x)
              (caddr x)
              (if (= (length x) 4)
                  (list-ref x 3)
                  #f))]
            [else (default-settings)]))
          
        (define/private (list-of-require-specs? l)
          (and (list? l)
               (andmap (λ (x)
                         (and (list? x)
                              (andmap (λ (x) (or (string? x) (symbol? x))) x)))
                       l)))
          
        (inherit get-allow-sharing? get-use-function-output-syntax? 
                 get-accept-quasiquote? get-read-accept-dot)
        (define/override (config-panel parent)
          (sharing/not-config-panel (get-allow-sharing?) (get-accept-quasiquote?) parent))
          
        (define/override (on-execute settings run-in-user-thread)
          (let ([drs-namespace (current-namespace)]
                [set-result-module-name 
                 ((current-module-name-resolver) '(lib "lang/private/set-result.ss") #f #f #t)]
                [scheme-test-module-name
                 ((current-module-name-resolver) '(lib "test-engine/racket-tests.ss") #f #f #t)]
                [scheme-signature-module-name
                 ((current-module-name-resolver) 
                  '(lib "deinprogramm/signature/signature-english.rkt") #f #f #t)]
                [tests-on? (with-handlers ([exn:unknown-preference? (λ (e) 'uninstalled)])
                             (preferences:get 'test-engine:enable?))])
            (run-in-user-thread
             (lambda ()
               (when (getenv "PLTDRHTDPNOCOMPILED") (use-compiled-file-paths '()))
               (read-accept-quasiquote (get-accept-quasiquote?))
               (namespace-attach-module drs-namespace ''drscheme-secrets)
               (namespace-attach-module drs-namespace set-result-module-name)                 
               (error-display-handler teaching-languages-error-display-handler)
               (error-value->string-handler 
                (λ (x y) (teaching-languages-error-value->string settings x y)))
               (current-eval (add-annotation (htdp-lang-settings-tracing? settings) (current-eval)))
               (error-print-source-location #f)
               (read-decimal-as-inexact #f)
               (read-accept-dot (get-read-accept-dot))
               (namespace-attach-module drs-namespace scheme-test-module-name)
               (namespace-require scheme-test-module-name)
               (namespace-attach-module drs-namespace scheme-signature-module-name)
               (namespace-require scheme-signature-module-name)
               (initialize-test-object!)
               ;; record signature violations with the test engine
               (signature-violation-proc
                (lambda (obj signature message blame)
                  (report-signature-violation! obj signature message blame)))
               ;; It the test engine plugin (test-engine/test-tool) is not
               ;; installed, still run the tests but don't connect to the
               ;; graphical interface. Instead, let the existing textual
               ;; interface do the work.
               (test-execute (and tests-on? #t))
               (unless (eq? tests-on? 'uninstalled)
                 (display-test-results-parameter
                  (lambda (markup)
                    (test-display-results! (drscheme:rep:current-rep)
                                           drs-eventspace
                                           markup))))
               (get-rewritten-error-message-parameter get-rewriten-error-message)
               (signature-checking-enabled?
                (get-preference 'signatures:enable-checking? (lambda () #t)))
               (render-value-parameter (λ (v)
                                         (let ([o (open-output-string)])
                                           (render-value/format v settings o 40)
                                           (get-output-string o)))))))
          (super on-execute settings run-in-user-thread)
            
          ;; set the global-port-print-handler after the super class because the super sets it too
          (run-in-user-thread
           (lambda ()
             (define my-setup-printing-parameters 
               (drscheme:language:make-setup-printing-parameters))
             (global-port-print-handler
              (λ (value port [depth 0])
                (teaching-language-render-value/format my-setup-printing-parameters
                                                       value settings port 'infinity))))))

        (define/private (teaching-languages-error-value->string settings v len)
          (let ([sp (open-output-string)])
            (set-printing-parameters settings (λ () (print v sp)))
            (flush-output sp)
            (let ([s (get-output-string sp)])
              (cond
                [(<= (string-length s) len) s]
                [else (string-append (substring s 0 (- len 3)) "...")]))))

        ;; set-printing-parameters : settings ( -> TST) -> TST
        ;; is implicitly exposed to the stepper.  watch out!  --  john
        (define/public (set-printing-parameters settings thunk)
          (define img-str "#<image>")
          (define (is-image? val)
            (or (is-a? val ic:image%)         ;; 2htdp/image
                (is-a? val cache-image-snip%) ;; htdp/image
                (is-a? val image-snip%)       ;; literal image constant
                (is-a? val bitmap%)))         ;; works in other places, so include it here too
          (define tfe-ids? (htdp-lang-settings-true/false/empty-as-ids? settings))
          (parameterize ([pc:booleans-as-true/false tfe-ids?]
                         [pc:add-make-prefix-to-constructor #t]
                         [print-boolean-long-form #t]
                         [pc:abbreviate-cons-as-list (get-abbreviate-cons-as-list)]
                         [pc:current-print-convert-hook
                          (let ([ph (pc:current-print-convert-hook)])
                            (lambda (val basic sub)
                              (cond
                                [(and (not tfe-ids?) (equal? val '())) ''()]
                                [(equal? val set!-result) '(void)]
                                [(signature? val)
                                 (or (signature-name val)
                                     '<signature>)]
				[(bytes? val)
				 (if (< (bytes-length val) 100)
				     val
				     (bytes-append (subbytes val 0 99) #"... truncated"))]
                                [else (ph val basic sub)])))]
                         [pretty-print-show-inexactness #t]
                         [pretty-print-exact-as-decimal #t]
                         [pretty-print-print-hook
                          (let ([oh (pretty-print-print-hook)])
                            (λ (val display? port)
                              (if (and (not (port-writes-special? port))
                                       (is-image? val))
                                  (begin (display img-str port)
                                         (string-length img-str))
                                  (oh val display? port))))]
                         [pretty-print-size-hook
                          (let ([oh (pretty-print-size-hook)])
                            (λ (val display? port)
                              (if (and (not (port-writes-special? port))
                                       (is-image? val))
                                  (string-length img-str)
                                  (oh val display? port))))]
                         [pc:use-named/undefined-handler
                          (lambda (x)
                            (and (get-use-function-output-syntax?)
                                 (procedure? x)
                                 (object-name x)))]
                         [pc:named/undefined-handler
                          (lambda (x)
                            (string->symbol
                             (format "function:~a" (object-name x))))])
            (thunk)))
          
        (define/override (render-value/format value settings port width)
          (teaching-language-render-value/format drscheme:language:setup-printing-parameters
                                                 value settings port width))
        (define/override (render-value value settings port)
          (teaching-language-render-value/format drscheme:language:setup-printing-parameters
                                                 value settings port 'infinity))
          
        (define/private (teaching-language-render-value/format setup-printing-parameters 
                                                               value settings port width)
          ;; set drscheme's printing parameters
          (setup-printing-parameters
           (λ ()
             ;; then adjust the settings for the teaching languages
             (set-printing-parameters
              settings
              (λ ()
                (let*-values ([(converted-value write?)
                               (call-with-values
                                (lambda ()
                                  (drscheme:language:simple-module-based-language-convert-value
                                   value settings))
                                (case-lambda
                                  [(converted-value) (values converted-value #t)]
                                  [(converted-value write?) (values converted-value write?)]))])
                  (let ([pretty-out (if write? pretty-write pretty-print)])
                    (cond
                      [(drscheme:language:simple-settings-insert-newlines settings)
                       (if (number? width)
                           (parameterize ([pretty-print-columns width])
                             (pretty-out converted-value port))
                           (pretty-out converted-value port))]
                      [else
                       (parameterize ([pretty-print-columns 'infinity])
                         (pretty-out converted-value port))
                       (newline port)]))))))
           settings
           width))
          
        (super-new)))
      
    ;; sharing/not-config-panel :  boolean boolean parent 
    ;;                         -> (case-> (-> settings) (settings -> void))
    ;; constructs the config-panel for a language without a sharing option.
    (define (sharing/not-config-panel allow-sharing-config? accept-quasiquote? _parent)
      (define parent (make-object vertical-panel% _parent))
      (define input-panel (new group-box-panel%
                               [parent parent]
                               [label (string-constant input-syntax)]
                               [alignment '(left center)]))
        
      (define output-panel (new group-box-panel%
                                [parent parent]
                                [label (string-constant output-syntax)]
                                [alignment '(left center)]))
               
      (define tp-group-box (new group-box-panel%
                                [label (string-constant teachpacks)]
                                [parent parent]
                                [alignment '(center top)]))
      (define tp-panel (new vertical-panel%
                            [parent tp-group-box]
                            [alignment '(center center)]
                            [stretchable-width #f]
                            [stretchable-height #f]))
        
      (define case-sensitive (make-object check-box%
                               (string-constant case-sensitive-label)
                               input-panel
                               void))
      (define insert-newlines (new check-box%
                                   [label (string-constant use-pretty-printer-label)]
                                   [parent output-panel]
                                   [callback void]))
      (define tracing (new check-box%
                           [parent output-panel]
                           [label (string-constant tracing-enable-tracing)]
                           [callback void]))

      (define radiobox-parent-panel (new vertical-panel%
                                         [parent output-panel]
                                         [stretchable-height #f]))
      (define radiobox-labels '())
      (define (mk-radiobox label choices)
        (define hp (new horizontal-panel%
                        [parent radiobox-parent-panel]
                        [stretchable-height #f]))
        (new horizontal-panel% [parent hp] [stretchable-width #t])
        (set! radiobox-labels (cons (new message% [label label] [parent hp])
                                    radiobox-labels))
        (new radio-box%
             [label #f]
             [parent hp]
             [choices choices]))

      (define output-style (mk-radiobox
                            (string-constant output-style-label)
                            (if accept-quasiquote?
                                (list (string-constant constructor-printing-style)
                                      (string-constant quasiquote-printing-style)
                                      (string-constant write-printing-style))
                                (list (string-constant constructor-printing-style)
                                      (string-constant write-printing-style)))))
      (define output-tfe (mk-radiobox
                          (string-constant true-false-empty-style-label)
                          (list (string-constant true-false-empty-style-read)
                                (string-constant true-false-empty-style-ids))))
      (define fraction-style
        (mk-radiobox (string-constant fraction-style)
                     (list (string-constant use-mixed-fractions)
                           (string-constant use-repeating-decimals))))

      (define show-sharing #f)

      (define (get-biggest-width lst)
        (for/fold ([s 0])
                  ([lab (in-list lst)])
          (define-values (w h) (send lab get-graphical-min-size))
          (max w s)))

      (let* ([rbs (list output-style output-tfe fraction-style)]
             [min-width (get-biggest-width rbs)])
        (for ([par (in-list rbs)])
          (send par min-width min-width)))
        
      (define tps '())
          
      (when allow-sharing-config?
        (set! show-sharing
              (new check-box%
                   [parent output-panel]
                   [label (string-constant sharing-printing-label)]
                   [callback void])))
        
      ;; set the characteristics of the GUI
      (send _parent set-alignment 'center 'center)
      (send parent stretchable-height #f)
      (send parent stretchable-width #f)
      (send parent set-alignment 'center 'center)
        
      (case-lambda
        [()
         (make-htdp-lang-settings
          (send case-sensitive get-value)
          (if accept-quasiquote?
              (case (send output-style get-selection)
                [(0) 'constructor]
                [(1) 'quasiquote]
                [(2) 'write])
              (case (send output-style get-selection)
                [(0) 'constructor]
                [(1) 'write]))
          (case (send fraction-style get-selection)
            [(0) 'mixed-fraction]
            [(1) 'repeating-decimal])
          (and allow-sharing-config? (send show-sharing get-value))
          (send insert-newlines get-value)
          'none
          (send tracing get-value)
          tps
          (equal? (send output-tfe get-selection) 1))]
        [(settings)
         (send case-sensitive set-value 
               (drscheme:language:simple-settings-case-sensitive settings))
         (send output-style set-selection
               (if accept-quasiquote?
                   (case (drscheme:language:simple-settings-printing-style settings)
                     [(constructor) 0]
                     [(quasiquote) 1]
                     [(print trad-write write) 2])
                   (case (drscheme:language:simple-settings-printing-style settings)
                     [(constructor) 0]
                     [(quasiquote) 0]
                     [(print trad-write write) 1])))
         (send fraction-style set-selection
               (case (drscheme:language:simple-settings-fraction-style settings)
                 [(mixed-fraction) 0]
                 [(repeating-decimal) 1]))
         (when allow-sharing-config?
           (send show-sharing set-value
                 (drscheme:language:simple-settings-show-sharing settings)))
         (send insert-newlines set-value 
               (drscheme:language:simple-settings-insert-newlines settings))
         (set! tps (htdp-lang-settings-teachpacks settings))
         (send tp-panel change-children (λ (l) '()))
         (if (null? tps)
             (new message%
                  [parent tp-panel]
                  [label (string-constant teachpacks-none)])
             (for-each
              (λ (tp) (new message% 
                           [parent tp-panel]
                           [label (format "~s" tp)]))
              tps))
         (send tracing set-value (htdp-lang-settings-tracing? settings))
         (send output-tfe set-selection
               (if (htdp-lang-settings-true/false/empty-as-ids? settings)
                   1
                   0))
         (void)]))
      
    (define simple-htdp-language%
      (class* drscheme:language:simple-module-based-language% (htdp-language<%>)
        (init-field sharing-printing
                    abbreviate-cons-as-list
                    allow-sharing?
                    manual
                    reader-module
                    (use-function-output-syntax? #f)
                    (accept-quasiquote? #t)
                    (read-accept-dot #f)
                    (style-delta #f))
        (define/public (get-sharing-printing) sharing-printing)
        (define/public (get-abbreviate-cons-as-list) abbreviate-cons-as-list)
        (define/public (get-allow-sharing?) allow-sharing?)
        (define/public (get-manual) manual)
        (define/public (get-use-function-output-syntax?) use-function-output-syntax?)
        (define/public (get-accept-quasiquote?) accept-quasiquote?)
        (define/public (get-read-accept-dot) read-accept-dot)
        ;(define/override (get-one-line-summary) one-line-summary)
        (define/public (get-htdp-style-delta) style-delta)
          
        (super-new [language-url "http://www.htdp.org/"])))
      
    (define (language-extension %)
      (class %
        (inherit get-manual)
          
        (define/override (extra-repl-information settings port) 
          (define welcome (drscheme:rep:get-welcome-delta))
          (define (go str sd)
            (let* ([s (make-object string-snip% str)]
                   [sl (editor:get-standard-style-list)]
                   [std (send sl find-named-style "Standard")]
                   [style (send sl find-or-create-style std sd)])
              (send s set-style style)
              (write-special s port)))
            
          (define tps (htdp-lang-settings-teachpacks settings))
            
          (unless (null? tps)
            (go "Teachpack" welcome)
            (cond
              [(= 1 (length tps))
               (go ": " welcome)
               (go (tp-require->str (car tps)) (drscheme:rep:get-dark-green-delta))]
              [(= 2 (length tps))
               (go "s: " welcome)
               (go (tp-require->str (car tps)) (drscheme:rep:get-dark-green-delta))
               (go " and " welcome)
               (go (tp-require->str (cadr tps)) (drscheme:rep:get-dark-green-delta))]
              [else
               (go "s: " welcome)
               (go (tp-require->str (car tps)) (drscheme:rep:get-dark-green-delta))
               (let loop ([these-tps (cdr tps)])
                 (cond
                   [(null? (cdr these-tps))
                    (go ", and " welcome)
                    (go (tp-require->str (car these-tps)) (drscheme:rep:get-dark-green-delta))]
                   [else
                    (go ", " welcome)
                    (go (tp-require->str (car these-tps)) (drscheme:rep:get-dark-green-delta))
                    (loop (cdr these-tps))]))])
            (go "." welcome)
            (newline port)))
          
        (define/override (first-opened settings)
          (define-values (mod name)
            (create-empty-module (get-module)
                                 (htdp-lang-settings-teachpacks settings)))
          (eval mod)
          (dynamic-require `',name #f)
          (current-namespace (module->namespace `',name)))
          
        (define/private (tp-require->str tp)
          (match tp
            [`(lib ,x) 
             (define m (regexp-match #rx"teachpack/(2?htdp/(.*))$" x))
             (cond
               [m
                (define long-name (list-ref m 1))
                (define short-name (list-ref m 2))
                (if (regexp-match #rx"htdp/image[.]" long-name)
                    long-name
                    short-name)]
               [else (format "~s" tp)])]
            [_ (format "~s" tp)]))
          
        (inherit get-module get-transformer-module get-init-code
                 use-namespace-require/copy?)
        (define/override (create-executable setting parent program-filename)
          (let ([dist-filename
                 (drscheme:language:put-executable
                  parent program-filename
                  'distribution
                  #t 
                  (string-constant save-a-mred-distribution))])
            (when dist-filename
              (drscheme:language:create-distribution-for-executable 
               dist-filename
               #t
               (λ (exe-name)
                 (create-htdp-lang-executable program-filename exe-name reader-module))))))

        (define/private (symbol-append x y)
          (string->symbol
           (string-append
            (symbol->string x)
            (symbol->string y))))
          
        (inherit get-htdp-style-delta)
        (define/override (get-style-delta)
          (get-htdp-style-delta))
          
        (inherit get-reader set-printing-parameters)
          
        (define/override (front-end/complete-program port settings)
          (expand-teaching-program port  
                                   (get-reader)
                                   (get-module)
                                   (htdp-lang-settings-teachpacks settings)))
          
        (define/override (front-end/interaction port settings)
          (let ([t (super front-end/interaction port settings)]
                [done? #f]
                [test-object (test-object-copy (current-test-object))])
            (λ ()
              (cond
                [done? eof]
                [else
                 (let ([ans (parameterize ([read-accept-lang #f])
                              (t))])
                   (cond
                     [(eof-object? ans)
                      (if (test-object=? test-object (current-test-object))
                          eof
                          (begin
                            ; only retest if something has changed
                            (set! done? #t)
                            #`(test)))]
                     [else ans]))]))))

        ;; obtain the list of modules that requested to be
        ;; searchable for HtDP
        (define extra-teachpack-modules
          (let ()
            (define pkgs (find-relevant-directories HTDP-TEACHPACK-INFO-KEYS))
            (define infos
              (for/list ([pkg (in-list pkgs)])
                (with-handlers ([exn:fail? (λ (e) #f)])
                  (get-info/full pkg))))
            (define modules
              (for*/list ([info (in-list infos)]
                          #:when (and info
                                      (list? (info HTDP-TEACHPACK-MODULE-KEY)))
                          [module-name (in-list (info HTDP-TEACHPACK-MODULE-KEY))])
                module-name))
            modules))


        (define keywords #f)
        (define/augment (capability-value key)
          (case key
            [(drscheme:autocomplete-words)
             (unless keywords 
               ;; complete with everything, which is wrong ..
               (set! keywords (text:get-completions/manuals #f))) 
             keywords]
            [(drscheme:teachpack-menu-items) htdp-teachpack-callbacks]
            [(drscheme:special:insert-lambda) #f]
            [(drscheme:help-context-term)
             (let* ([m (get-module)]
                    [m (and m (pair? m) (pair? (cdr m)) (cadr m))]
                    [m (and m (regexp-match #rx"^(lang/[^/.]+).ss$" m))]
                    [m (and m (cadr m))])
               (if m
                   (format "O:{ L:~a T:teachpack T:picturing-programs ~a }"
                           m
                           (string-join
                            (for/list ([extra-module (in-list extra-teachpack-modules)])
                              (format "M:~a" extra-module))
                            " "))
                   (error 'drscheme:help-context-term
                          "internal error: unexpected module spec")))]
            [(tests:test-menu tests:dock-menu) #t]
            [else (inner (drscheme:language:get-capability-default key) 
                         capability-value
                         key)]))
          
        (define htdp-teachpack-callbacks
          (drscheme:unit:make-teachpack-callbacks
           (λ (settings) 
             (map (λ (x) (tp-require->str x))
                  (htdp-lang-settings-teachpacks settings)))
           (λ (settings parent) 
             (define old-tps (htdp-lang-settings-teachpacks settings))
             (define tp-dirs (list "htdp" "2htdp"))
             (define labels (list (string-constant teachpack-pre-installed/htdp)
                                  (string-constant teachpack-pre-installed/2htdp)))
             (define tp-syms '(htdp-teachpacks 2htdp-teachpacks))
             (define-values (tp-to-remove tp-to-add)
               (get-teachpack-from-user parent tp-dirs labels tp-syms old-tps))
             (define new-tps (let ([removed (if tp-to-remove
                                                (remove tp-to-remove old-tps)
                                                old-tps)])
                               (if (or (not tp-to-add) (member tp-to-add old-tps))
                                   removed
                                   (append removed (list tp-to-add)))))
             (when (member tp-to-add old-tps)
               (message-box (string-constant drscheme)
                            (format (string-constant already-added-teachpack)
                                    (tp-require->str tp-to-add))
                            #:dialog-mixin frame:focus-table-mixin))
             (preferences:set 'drracket:htdp:last-set-teachpacks/multi-lib new-tps)
             (make-htdp-lang-settings
              (drscheme:language:simple-settings-case-sensitive settings)
              (drscheme:language:simple-settings-printing-style settings)
              (drscheme:language:simple-settings-fraction-style settings)
              (drscheme:language:simple-settings-show-sharing settings)
              (drscheme:language:simple-settings-insert-newlines settings)
              (drscheme:language:simple-settings-annotations settings)
              (htdp-lang-settings-tracing? settings)
              new-tps
              (htdp-lang-settings-true/false/empty-as-ids? settings)))
           (λ (settings name) 
             (let ([new-tps (filter (λ (x) (not (equal? (tp-require->str x) name)))
                                    (htdp-lang-settings-teachpacks settings))])
               (preferences:set 'drracket:htdp:last-set-teachpacks/multi-lib new-tps)
               (make-htdp-lang-settings
                (drscheme:language:simple-settings-case-sensitive settings)
                (drscheme:language:simple-settings-printing-style settings)
                (drscheme:language:simple-settings-fraction-style settings)
                (drscheme:language:simple-settings-show-sharing settings)
                (drscheme:language:simple-settings-insert-newlines settings)
                (drscheme:language:simple-settings-annotations settings)
                (htdp-lang-settings-tracing? settings)
                new-tps
                (htdp-lang-settings-true/false/empty-as-ids? settings))))
           (λ (settings) 
             (preferences:set 'drracket:htdp:last-set-teachpacks/multi-lib '())
             (make-htdp-lang-settings
              (drscheme:language:simple-settings-case-sensitive settings)
              (drscheme:language:simple-settings-printing-style settings)
              (drscheme:language:simple-settings-fraction-style settings)
              (drscheme:language:simple-settings-show-sharing settings)
              (drscheme:language:simple-settings-insert-newlines settings)
              (drscheme:language:simple-settings-annotations settings)
              (htdp-lang-settings-tracing? settings)
              '()
              (htdp-lang-settings-true/false/empty-as-ids? settings)))))
        
        (inherit-field reader-module)
        (define/override (get-reader-module) reader-module)
        (define/override (get-metadata modname settings)
          (define parsed-tps 
            (marshall-teachpack-settings
             (htdp-lang-settings-teachpacks settings)))
          (string-append
           (apply string-append
                  (map (λ (x) (string-append x "\n"))
                       htdp-save-file-prefix))
           (format "#reader~s~s\n"
                   reader-module
                   `((modname ,modname)
                     (read-case-sensitive
                      ,(drscheme:language:simple-settings-case-sensitive settings))
                     (teachpacks ,parsed-tps)
                     (htdp-settings
                      ,(for/vector ([e (in-vector (htdp-lang-settings->vector settings))]
                                    [i (in-naturals)])
                         (cond
                           [(= i teachpacks-field-index) parsed-tps]
                           [else e])))))))
          
        (inherit default-settings)
        (define/override (metadata->settings metadata)
          (define table (massage-metadata (metadata->table metadata)))
          (define ssv (assoc 'htdp-settings table))
          (cond
            [ssv
             (define settings-list (vector->list (cadr ssv)))
             (define settings-list-len (length settings-list))
             (cond
               [(or (equal? settings-list-len
                            (procedure-arity make-htdp-lang-settings))
                    (equal? settings-list-len
                            (- (procedure-arity make-htdp-lang-settings) 1)))
                (define new-settings-list
                  (for/list ([i (in-naturals)]
                             [e (in-list settings-list)])
                    (cond
                      [(= i teachpacks-field-index)
                       (unmarshall-teachpack-settings e)]
                      [else e])))
                (if (= settings-list-len (procedure-arity make-htdp-lang-settings))
                    (apply make-htdp-lang-settings new-settings-list)
                    (apply make-htdp-lang-settings (append new-settings-list '(#f))))]
               [else
                (default-settings)])]
            [else (default-settings)]))
          
        ;; these are used for the benefit of v5.3.6 and earlier drracket's
        ;; specifically, those language doesn't work right with teachpack
        ;; paths of the form (lib "a/b/c.rkt"), but they do with ones of the
        ;; form (lib "c.rkt" "a" "b"), so we do that conversion here when
        ;; sending out a file that might go into 5.3.6.
          
        (define/private (unmarshall-teachpack-settings obj)
          (cond
            [(list? obj)
             (for/list ([obj (in-list obj)])
               (match obj
                 [`(lib ,(? string? s1) ,(? string? s2) ...)
                  `(lib ,(apply string-append (add-between (append s2 (list s1)) "/")))]
                 [else obj]))]
            [else obj]))
          
        (define/private (marshall-teachpack-settings obj)
          (define (has-slashes? s) (regexp-match? #rx"/" s))
          (cond
            [(list? obj) 
             (for/list ([obj (in-list obj)])
               (match obj
                 [`(lib ,(? (and/c string? has-slashes?) s))
                  (define split (regexp-split #rx"/" s))
                  `(lib ,(last split) ,@(reverse (cdr (reverse split))))]
                 [else obj]))]
            [else obj]))
          
        (define/private (massage-metadata md)
          (if (and (list? md)
                   (andmap (λ (x) (and (pair? x) (symbol? (car x)))) md))
              md
              '()))
          
        (define/private (metadata->table metadata)
          (with-handlers ((exn:fail:read? (λ (x) #f)))
            (let ([p (open-input-string metadata)])
              (regexp-match #rx"\n#reader" p) ;; skip to reader
              (read p) ;; skip module
              (read p))))
          
        (define/override (get-metadata-lines) 3)
          
        (super-new)))
      
      

    (define (stepper-settings-language %)
      (if (implementation? % stepper-language<%>)
          (class* % (stepper-language<%>)
            (init-field stepper:supported)
            (init-field stepper:enable-let-lifting)
            (init-field stepper:show-lambdas-as-lambdas)
            (define/override (stepper:supported?) stepper:supported)
            (define/override (stepper:enable-let-lifting?) stepper:enable-let-lifting)
            (define/override (stepper:show-lambdas-as-lambdas?) stepper:show-lambdas-as-lambdas)
            (super-new))
          (class* % ()
            (init stepper:supported)
            (init stepper:enable-let-lifting)
            (init stepper:show-lambdas-as-lambdas)
            (super-new))))

    (define (debugger-settings-language %)
      (if (implementation? % debugger-language<%>)
          (class* % (debugger-language<%>)
            (init-field [debugger:supported #f])
            (define/override (debugger:supported?) debugger:supported)
            (super-new))
          (class %
            (init [debugger:supported #f])
            (super-new))))

    ;; filter/hide-ids : syntax[list] -> listof syntax
    (define (filter/hide-ids ids)
      ;; When a `define-values' or `define-syntax' declaration
      ;; is macro-generated, if the defined name also originates
      ;; from a macro, then the name is hidden to anything
      ;; that wasn't generated by the same macro invocation. This
      ;; hiding relies on renaming at the symbol level, and it's
      ;; exposed by the fact that `syntax-e' of the identifier 
      ;; returns a different name than `identifier-binding'.
      (filter
       (lambda (id)
         (let ([ib (identifier-binding id)])
           ;; ib should always be a 4-elem list, but
           ;; check, just in case:
           (or (not (pair? ib)) 
               (eq? (syntax-e id)
                    (cadr ib)))))
       (syntax->list ids)))
                 
      
    ;                                                                                              
    ;                                                                                              
    ;                                                                                              
    ;   ;                                                             ;                     ;      
    ;   ;                                                             ;                     ;      
    ;   ;                                                             ;                     ;      
    ;   ; ;;    ;;;    ; ;;     ;; ;   ; ;;  ;;    ;;;    ; ;;        ; ;;    ;;;     ;;;   ;   ;  
    ;   ;;  ;  ;   ;   ;;  ;   ;  ;;   ;;  ;;  ;  ;   ;   ;;  ;       ;;  ;  ;   ;   ;   ;  ;  ;   
    ;   ;   ;      ;   ;   ;  ;    ;   ;   ;   ;      ;   ;   ;       ;   ;      ;  ;       ; ;    
    ;   ;   ;   ;;;;   ;   ;  ;    ;   ;   ;   ;   ;;;;   ;   ;       ;   ;   ;;;;  ;       ;;;    
    ;   ;   ;  ;   ;   ;   ;  ;    ;   ;   ;   ;  ;   ;   ;   ;       ;   ;  ;   ;  ;       ;  ;   
    ;   ;   ;  ;   ;   ;   ;   ;  ;;   ;   ;   ;  ;   ;   ;   ;       ;   ;  ;   ;   ;   ;  ;   ;  
    ;   ;   ;   ;;;;;  ;   ;    ;; ;   ;   ;   ;   ;;;;;  ;   ;       ;   ;   ;;;;;   ;;;   ;    ; 
    ;                              ;                                                               
    ;                         ;    ;                                                               
    ;                          ;;;;                                                                
      

    ;; this inspector should be powerful enough to see
    ;; any structure defined in the user's namespace
    (define drscheme-inspector (current-inspector))
    (eval `(,#'module drscheme-secrets mzscheme
                      (provide drscheme-inspector)
                      (define drscheme-inspector ,drscheme-inspector)))
    (namespace-require ''drscheme-secrets)
      
      
      
    ;                                                               
    ;                                                               
    ;                                                               
    ;                                                               
    ;                                                               
    ;                                 ;                             
    ;    ;;;   ; ;  ; ;   ;;;    ; ; ;;;;  ; ;  ;;;     ;;;    ;;;  
    ;   ;   ;  ;;   ;;   ;   ;   ;;   ;    ;;  ;   ;   ;   ;  ;   ; 
    ;  ;    ;  ;    ;   ;     ;  ;    ;    ;       ;  ;      ;    ; 
    ;  ;;;;;;  ;    ;   ;     ;  ;    ;    ;    ;;;;  ;      ;;;;;; 
    ;  ;       ;    ;   ;     ;  ;    ;    ;   ;   ;  ;      ;      
    ;   ;      ;    ;    ;   ;   ;    ;    ;   ;   ;   ;   ;  ;     
    ;    ;;;;  ;    ;     ;;;    ;     ;;  ;    ;;;;;   ;;;    ;;;; 
    ;                                                               
    ;                                                               
    ;                                                               
      
      
      
      
    (define mf-note
      (let ([bitmap (make-object bitmap% (collection-file-path "mf.gif" "icons"))])
        (and (send bitmap ok?)
             (make-object image-snip% bitmap))))
    
    ;; teaching-languages-error-display-handler : 
    ;;    (string (union TST exn) -> void) -> string exn -> void
    ;; adds in the bug icon, if there are contexts to display
    (define (teaching-languages-error-display-handler msg exn)
      (if (exn? exn)
          (display (get-rewriten-error-message exn) (current-error-port))
          (eprintf "uncaught exception: ~e" exn))
      (eprintf "\n")

      ;; need to flush here so that error annotations inserted in next line
      ;; don't get erased if this output were to happen after the insertion
      (flush-output (current-error-port))
          
      [define rep (drscheme:rep:current-rep)]
      (when (and (is-a? rep drscheme:rep:text<%>) (eq? (send rep get-err-port) (current-error-port)))
        [define to-highlight 
          (cond
            [(exn:srclocs? exn) ((exn:srclocs-accessor exn) exn)]
            [(not (exn? exn)) '()]
            [else ; (exn? exn) 

             ;; continuation-mark -> boolean?
             ;; is tihs a good path for highlighting? 
             (define (good? mark)
               (cond
                 [(boolean? mark) #f]
                 [else (define ppath (car mark))
                       (or (and (path? ppath)
                                ;; exclude paths that result from macro expansion,
                                ;; specifically define-record-procedures
                                ;; see racket/drracket#157
                                ;; MF: I added this as adviced by Mike S.,
                                ;; but I am not sure I need this at all. 
                                (not (let ([rel (path->collects-relative ppath)])
                                       (and (pair? rel)
                                            (eq? 'collects (car rel))
                                            (equal? #"lang" (cadr rel))))))
                           (symbol? ppath))]))
             
             [define cms (exn-continuation-marks exn)]
             [define lcm (continuation-mark-set->list cms teaching-languages-continuation-mark-key)]
             
             (cond
               [(not lcm) '()] ;; MF: I don't understand how this could possibly hold
               [(for/first ((m lcm) #:when (good? m)) (list (apply make-srcloc m))) => values]
               [else '()])])]

        (parameterize ([current-eventspace drs-eventspace])
          (queue-callback
           (lambda ()
             ;; need to make sure that the user's eventspace is still the same and still running here?
             (send rep highlight-errors to-highlight #f))))))
      
    ;; with-mark : syntax syntax exact-nonnegative-integer -> syntax
    ;; a member of stacktrace-imports^
    ;; guarantees that the continuation marks associated with
    ;; teaching-languages-continuation-mark-key are members of the debug-source type
    (define (with-mark source-stx expr phase)
      [define source (syntax-source source-stx)]
      [define line   (syntax-line source-stx)]
      [define col    (syntax-column source-stx)]
      [define alpha  (syntax-position source-stx)]
      [define span   (syntax-span source-stx)]
      (define delta  (- phase base-phase))
      (if (and (or (symbol? source) (path? source)) (number? alpha) (number? span))
          (with-syntax ([expr  expr]
                        [mark  (list source line col alpha span)]
                        [tlcmk teaching-languages-continuation-mark-key]
                        [wcm   (syntax-shift-phase-level #'with-continuation-mark delta)]
                        [quot  (syntax-shift-phase-level #'quote delta)])
            #`(wcm (quot tlcmk) (quot mark) expr))
          expr))

    (define base-phase (variable-reference->module-base-phase (#%variable-reference)))
      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;  profiling infrastructure. Not used.
    ;;
      
    (define profile-key (gensym))
    (define (profiling-enabled) #f)
    (define (initialize-profile-point . x) (void))
    (define (register-profile-start . x) #f)
    (define (register-profile-done . x) (void))
      
      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;;  test coverage
    ;;
      
    (define test-coverage-enabled (make-parameter #t))
    (define current-test-coverage-info (make-thread-cell #f))
      
    (define (initialize-test-coverage-point expr)
      (unless (thread-cell-ref current-test-coverage-info)
        (let ([ht (make-hasheq)])
          (thread-cell-set! current-test-coverage-info ht)
          (let ([rep (drscheme:rep:current-rep)])
            (when rep
              (parameterize ([current-eventspace drs-eventspace])
                (queue-callback
                 (λ ()
                   (define sl (editor:get-standard-style-list))
                   (define on-s (send sl find-named-style test-coverage-on-style-name))
                   (define off-s (send sl find-named-style test-coverage-off-style-name))
                   (send rep set-test-coverage-info ht on-s off-s #f))))))))
      (let ([ht (thread-cell-ref current-test-coverage-info)])
        (when ht
          (hash-set! ht expr #;(box #f) (mcons #f #f)))))
      
    (define (test-covered expr)
      (let* ([ht (or (thread-cell-ref current-test-coverage-info)
                     (error 'htdp-langs
                            "internal-error: no test-coverage table"))]
             [v (hash-ref ht expr
                          (lambda ()
                            (error 'htdp-langs
                                   "internal-error: expression not found: ~.s"
                                   expr)))])
        #; (lambda () (set-box! v #t))
        (with-syntax ([v v]) #'(#%plain-app set-mcar! v #t))))
      
    (define-values/invoke-unit et:stacktrace@
      (import et:stacktrace-imports^) (export (prefix et: et:stacktrace^)))
     
    ;; add-annotation : boolean (sexp -> value) -> sexp -> value
    ;; adds debugging and test coverage information to `sexp' and calls `oe'
    (define (add-annotation tracing? oe)
      (let ([teaching-language-eval-handler
             (lambda (exp)
               (let* ([is-compiled? (compiled-expression? (if (syntax? exp) (syntax-e exp) exp))]
                      [annotated
                       (if is-compiled? 
                           exp
                           (let* ([et-annotated (et:annotate-top (expand exp) 
                                                                 (namespace-base-phase))]
                                  [tr-annotated
                                   (if tracing?
                                       (drscheme:tracing:annotate (expand et-annotated))
                                       et-annotated)])
                             tr-annotated))])
                 (oe annotated)))])
        teaching-language-eval-handler))
      
     
   
    ;                                                                                               
    ;                                                                                               
    ;                                                                                               
    ;                                                                                               
    ;                    ;      ;;;   ;        ;                            ;  ;;;                  
    ;                  ;;;          ;;;      ;;;                          ;;;  ;;;                  
    ;  ;;; ;;  ;;; ;;; ;;;;     ;;; ;;;;     ;;;;   ;;;    ;; ;;;   ;;;;  ;;;; ;;; ;;    ;;;;  ;;; ;
    ;  ;;;;;;; ;;; ;;; ;;;;     ;;; ;;;;     ;;;;  ;;;;;  ;;;;;;;  ;; ;;; ;;;; ;;;;;;;  ;; ;;; ;;;;;
    ;  ;;; ;;; ;;; ;;; ;;;      ;;; ;;;      ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;;  
    ;  ;;; ;;; ;;; ;;; ;;;      ;;; ;;;      ;;;  ;;; ;;; ;;; ;;; ;;;;;;; ;;;  ;;; ;;; ;;;;;;; ;;;  
    ;  ;;; ;;; ;;; ;;; ;;;      ;;; ;;;      ;;;  ;;; ;;; ;;; ;;; ;;;     ;;;  ;;; ;;; ;;;     ;;;  
    ;  ;;;;;;; ;;;;;;; ;;;;     ;;; ;;;;     ;;;;  ;;;;;  ;;;;;;;  ;;;;;; ;;;; ;;; ;;;  ;;;;;; ;;;  
    ;  ;;; ;;   ;; ;;;  ;;;     ;;;  ;;;      ;;;   ;;;    ;; ;;;   ;;;;   ;;; ;;; ;;;   ;;;;  ;;;  
    ;  ;;;                                                    ;;;                                   
    ;  ;;;                                                ;;;;;;                                    
    ;                                                                                               
    ;                                                                                               

      
    ;; add-htdp-language : (instanceof htdp-language<%>) -> void
    (define (add-htdp-language o)
      (drscheme:language-configuration:add-language 
       o
       #:allow-executable-creation? #t))
      
    (define (phase1) (void))

    ;; phase2 : -> void
    (define (phase2)
      (define htdp-language%
        (stepper-settings-language
         (debugger-settings-language
          ((drscheme:language:get-default-mixin)
           (language-extension
            (drscheme:language:module-based-language->language-mixin
             (module-based-language-extension
              (drscheme:language:simple-module-based-language->module-based-language-mixin
               simple-htdp-language%))))))))
        
      (add-htdp-language
       (instantiate htdp-language% ()
         (one-line-summary (string-constant advanced-one-line-summary))
         (module '(lib "lang/htdp-advanced.ss"))
         (manual #"advanced")
         (language-position
          (list (string-constant teaching-languages)
                (string-constant how-to-design-programs)
                (string-constant advanced-student)))
         (language-id "plt:advanced-student")
         (language-numbers '(-500 -500 5))
         (sharing-printing #t)
         (abbreviate-cons-as-list #t)
         (allow-sharing? #t)
         (reader-module '(lib "htdp-advanced-reader.ss" "lang"))
         (debugger:supported #t)
         (stepper:supported #f)
         (stepper:enable-let-lifting #t)
         (stepper:show-lambdas-as-lambdas #t)))
        
      (add-htdp-language
       (instantiate htdp-language% ()
         (one-line-summary (string-constant intermediate/lambda-one-line-summary))
         (module '(lib "lang/htdp-intermediate-lambda.ss"))
         (manual #"intermediate-lambda")
         (language-position
          (list (string-constant teaching-languages)
                (string-constant how-to-design-programs)
                (string-constant intermediate-student/lambda)))
         (language-id "plt:intermediate-student/lambda")
         (style-delta (let ([match (regexp-match-positions
                                    "lambda"
                                    (string-constant intermediate-student/lambda))])
                        (if match
                            (let ([pos (car match)])
                              (list (list (make-object style-delta% 'change-family 'modern)
                                          (car pos)
                                          (cdr pos))))
                            #f)))
         (language-numbers '(-500 -500 4))
         (sharing-printing #f)
         (abbreviate-cons-as-list #t)
         (allow-sharing? #f)
         (reader-module '(lib "htdp-intermediate-lambda-reader.ss" "lang"))
         (stepper:supported #t)
         (stepper:enable-let-lifting #t)
         (stepper:show-lambdas-as-lambdas #t)))
        
      (add-htdp-language
       (instantiate htdp-language% ()
         (one-line-summary (string-constant intermediate-one-line-summary))
         (module '(lib "lang/htdp-intermediate.ss"))
         (manual #"intermediate")
         (language-position
          (list (string-constant teaching-languages)
                (string-constant how-to-design-programs)
                (string-constant intermediate-student)))
         (language-id "plt:intermediate-student")
         (language-numbers '(-500 -500 3))
         (sharing-printing #f)
         (abbreviate-cons-as-list #t)
         (allow-sharing? #f)
         (use-function-output-syntax? #t)
         (reader-module '(lib "htdp-intermediate-reader.ss" "lang"))
         (stepper:supported #t)
         (stepper:enable-let-lifting #t)
         (stepper:show-lambdas-as-lambdas #f)))
        
      (add-htdp-language
       (instantiate htdp-language% ()
         (one-line-summary (string-constant beginning/abbrev-one-line-summary))
         (module '(lib "lang/htdp-beginner-abbr.ss"))
         (manual #"beginning-abbr")
         (language-position
          (list (string-constant teaching-languages)
                (string-constant how-to-design-programs)
                (string-constant beginning-student/abbrev)))
         (language-id "plt:beginning-student/abbrev")
         (language-numbers '(-500 -500 2))
         (sharing-printing #f)
         (abbreviate-cons-as-list #t)
         (allow-sharing? #f)
         (reader-module '(lib "htdp-beginner-abbr-reader.ss" "lang"))
         (stepper:supported #t)
         (stepper:enable-let-lifting #t)
         (stepper:show-lambdas-as-lambdas #f)))
        
      (add-htdp-language
       (instantiate htdp-language% ()
         (one-line-summary (string-constant beginning-one-line-summary))
         (module '(lib "lang/htdp-beginner.ss"))
         (manual #"beginning")
         (language-position
          (list (string-constant teaching-languages)
                (string-constant how-to-design-programs)
                (string-constant beginning-student)))
         (language-numbers '(-500 -500 1))
         (language-id "plt:beginning-student")
         (sharing-printing #f)
         (abbreviate-cons-as-list #f)
         (allow-sharing? #f)
         (accept-quasiquote? #f)
         (reader-module '(lib "htdp-beginner-reader.ss" "lang"))
         (stepper:supported #t)
         (stepper:enable-let-lifting #t)
         (stepper:show-lambdas-as-lambdas #f))))
      
    (define test-coverage-on-style-name "plt:htdp:test-coverage-on")
    (define test-coverage-off-style-name "plt:htdp:test-coverage-off")
    (define test-coverage-on-style-pref (string->symbol test-coverage-on-style-name))
    (define test-coverage-off-style-pref (string->symbol test-coverage-off-style-name))
      
    (color-prefs:add-color-scheme-entry test-coverage-on-style-pref
                                        #:style test-coverage-on-style-name
                                        "black"
                                        "white")
    (color-prefs:add-color-scheme-entry test-coverage-off-style-pref
                                        #:style test-coverage-off-style-name
                                        "orange"
                                        "indianred"
                                        #:background "black")
    (color-prefs:add-to-preferences-panel 
     "HtDP Languages"
     (λ (parent)
       (color-prefs:build-color-selection-panel parent
                                                test-coverage-on-style-pref
                                                test-coverage-on-style-name
                                                (string-constant test-coverage-on))
       (color-prefs:build-color-selection-panel parent
                                                test-coverage-off-style-pref
                                                test-coverage-off-style-name
                                                (string-constant test-coverage-off)
                                                #:background? #t)))))
