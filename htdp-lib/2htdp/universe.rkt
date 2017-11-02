#lang racket/gui

;; ---------------------------------------------------------------------------------------------------
;; the universe library provides the functionality to create interactive and distributed FPs in HtDP

;; DONT USE ___to-draw___ IN THIS FILE 

#| TODO: 
   -- run callbacks in user eventspace
   -- make timer fire just once; restart after on-tick callback finishes
          [on-tick tick-handler tick-producer tick-limit]
          tick-producer: World -> PositiveNumber 
          allow the time span to be a function of the world state 
   -- take out counting; replace by 0.25 delay

   -- make window resizable :: why
|#

(require (for-syntax syntax/parse
                     "private/clauses-spec-and-process.rkt"
                     stepper/private/syntax-property)
         (only-in syntax/location quote-srcloc)
         (only-in test-engine/racket-tests check-expect-maker)
         (only-in test-engine/test-engine test-format)
         (only-in (lib "test-engine/test-info.scm") make-unequal)
         "private/define-keywords.rkt"
         "private/clauses-spec-aux.rkt" 
         ;; ---
         "private/world.rkt"
         "private/universe.rkt"
         "private/universe-image.rkt"
         ;; 
         (only-in "private/launch-many-worlds.rkt" launch-many-worlds launch-many-worlds/proc)
         (only-in "private/stop.rkt" make-stop-the-world)
         (only-in "private/check-aux.rkt" sexp? SQPORT)
         (only-in "private/pad.rkt" pad-event? pad=?)
         htdp/error
         (rename-in lang/prim (first-order->higher-order f2h)))

(define-primitive stop-with make-stop-the-world)

(provide stop-with) ;; World -> STOP

(provide
 ;; (launch-many-worlds e1 ... e2)
 ;; run expressions e1 through e2 in parallel, produce all values in same order
 launch-many-worlds
 ;; launch-many-worlds/proc : (-> Any) *-> [Listof Any]
 launch-many-worlds/proc
 )

(provide-primitive
 sexp?  ;; Any -> Boolean 
 )

(define check-big-bang-continuation-key
  (make-continuation-mark-key
   'check-big-bang-continuation-key))

(define new-world (create-world world0))
(define new-universe (create-universe universe0))

(define-keywords AllSpec '() define-all
  ;; -- on-tick must specify a tick handler: World -> World 
  ;; it may specify a clock-tick rate
  [on-tick DEFAULT #'#f
           (function-with-arity
            1
            #:except
            [(_ f rate)
             #'(list
                (proc> 'on-tick (f2h f) 1)
                (num> 'on-tick rate (lambda (x) (and (real? x) (positive? x)))
                      "positive number" "rate"))]
            [(_ f rate limit)
             #'(list
                (proc> 'on-tick (f2h f) 1)
                (num> 'on-tick rate (lambda (x) (and (real? x) (positive? x)))
                      "positive number" "rate")
                (num> 'on-tick limit (lambda (x) (and (integer? x) (positive? x)))
                      "positive integer" "limit"))])]
  ;; -- state specifies whether to display the current state 
  [state DEFAULT #'#f (expr-with-check any> "expected a boolean or a string")]
  ;; Any -> Boolean 
  ;; -- check-with: all states should specify this predicate 
  [check-with DEFAULT #'True (function-with-arity 1)]
  ;; Natural
  ;; -- port: specify the port to use
  [port DEFAULT #'SQPORT (expr-with-check port> "expected a port number")])

;  (create-world world0)
(define-keywords WldSpec AllSpec create-world
  ;; (U #f (World -> Scene) (list (World -> Scene) Nat Nat))
  ;; on-draw must specify a rendering function; 
  ;;   it may specify dimensions
  [on-draw to-draw DEFAULT #'#f
           (function-with-arity
            1
            #:except
            [(_ f width height)
             #'(list (proc> 'to-draw (f2h f) 1)
                     (nat> 'to-draw width "width")
                     (nat> 'to-draw height "height"))])]
  ;; World Nat Nat MouseEvent -> World 
  ;; on-mouse must specify a mouse event handler 
  [on-mouse DEFAULT #f (function-with-arity 4)]
  ;; (U #f (World KeyEvent -> World))
  ;; on-key must specify a key event handler 
  [on-key DEFAULT #f (function-with-arity 2)]
  ;; (U #f (World PadEvent -> World))
  ;; on-pad must specify a pad event handler 
  [on-pad DEFAULT #f (function-with-arity 2)]
  ;; (U #f (World KeyEvent -> World))
  ;; on-release must specify a release event handler 
  [on-release DEFAULT #f (function-with-arity 2)]
  ;; (World S-expression -> World)
  ;; -- on-receive must specify a receive handler 
  [on-receive DEFAULT #'(lambda (w m) w) (function-with-arity 2)]
  ;; World -> Boolean 
  ;; -- stop-when must specify a predicate; it may specify a rendering function
  [stop-when DEFAULT #'False
             (function-with-arity
              1
              #:except
              [(_ stop? last-picture)
               #'(list (proc> 'stop-when (f2h stop?) 1)
                       (proc> 'stop-when (f2h last-picture) 1 #:place "second"))])]
  ;; (U #f Any)
  ;; -- should the session be recorded and turned into PNGs and an animated GIF
  ;; -- if the value is a string and is the name of a local directory, use it! 
  [record? DEFAULT #'#f (expr-with-check any> "")]
  ;; Boolean
  ;; -- close-on-stop: close the window when the program shuts down
  ;; in principle, a big-bang that specifies both
  ;;   [record? #true]
  ;; and
  ;;   [close-on-stop #true]
  ;; is self-contradictory; I will wait until someone complaints -- MF, 22 Nov 2015
  [close-on-stop DEFAULT #'#f (expr-with-check opt-nat> "expected a boolean or a natural number")]
  
  [display-mode DEFAULT #''normal
                (expr-with-check display-mode> "expected a display mode ('normal, 'fullscreen)")]
  ;; (U #f String)
  ;; -- name specifies one string 
  [name DEFAULT #'#f (expr-with-check string-or-symbol> "expected a string")]
  ;; (U #f IP)  
  ;; -- register must specify the internet address of a host (e.g., LOCALHOST)
  [register DEFAULT #'#f (expr-with-check ip> "expected a host (ip address)")])

;  (create-universe universe0)
(define-keywords UniSpec AllSpec create-universe
  ;; -- on-new must specify what happens when a world joins the universe
  [on-new 
   DEFAULT #'"my-bad"
   (function-with-arity 2)]
  ;; -- on-msg must specify what happens to a message from a world 
  [on-msg
   DEFAULT #'"my-bad"
   (function-with-arity 3)]
  ;; -- on-disconnect may specify what happens when a world drops out
  [on-disconnect
   ;; ******************************************************************
   DEFAULT #'(lambda (u w) (make-bundle u '() '()))
   ;; this is the wrong default function 
   ;; instead of K there should be a function that produces a bundle 
   (function-with-arity 2)
   ;; ******************************************************************
   ]
  ;; -- to-string specifies how to render the universe as a string for display
  [to-string
   DEFAULT #'#f
   (function-with-arity 1)])


;                                     
;                                     
;                                     
;   ;   ;                  ;        ; 
;   ;   ;                  ;        ; 
;   ;   ;                  ;        ; 
;   ;   ;   ;;;   ; ;;     ;     ;;;; 
;   ;   ;  ;   ;  ;;  ;    ;    ;   ; 
;   ; ; ;  ;   ;  ;   ;    ;    ;   ; 
;   ;; ;;  ;   ;  ;        ;    ;   ; 
;   ;   ;  ;   ;  ;        ;    ;   ; 
;   ;   ;   ;;;   ;        ;;    ;;;; 
;                                     
;                                     
;                                     

(provide 
 big-bang     ;; <syntax> : see below
 pad-handler  ;; <syntax> : see below
 check-big-bang
 check-big-bang*
 )

(provide-primitives
 make-package  ;; World Sexp -> Package
 package?      ;; Any -> Boolean 
 run-movie     ;; [r Positive] [m [Listof Image]] -> true
 ;; run movie m at rate r images per second 
 mouse-event?  ;; Any -> Boolean : MOUSE-EVTS
 mouse=?       ;; MOUSE-EVTS MOUSE-EVTS -> Boolean 
 key-event?    ;; Any -> Boolean : KEY-EVTS
 key=?         ;; KEY-EVTS KEY-EVTS -> Boolean
 pad-event?    ;; KeyEvent -> Boolean
 ;; is the given key-event also a pad-event? 
 pad=?         ;; PadEvent PadEvent -> Boolean
 ;; Events for check-big-bang
 make-event-expect ;; BigBangEvent World -> (EventExpectof World)
 make-tick          ;; -> BigBangEvent
 make-key           ;; KeyEvent -> BigBangEvent
 make-pad           ;; PadEvent -> BigBangEvent
 make-release       ;; ReleaseEvent -> BigBangEvent
 make-mouse         ;; Integer Integer MouseEvent -> BigBangEvent
 make-receive       ;; S-Exp -> BigBangEvent
 ;; ---
 ;; IP : a string that points to a machine on the net 
 )

(provide LOCALHOST     ;; IP
         )

(provide-higher-order-primitive
 run-simulation (create-scene) ; (Nat -> Scene) -> Nat
 )

(provide-higher-order-primitive
 animate (create-scene) ; (Nat -> Scene) -> Nat
 )

(define MOUSE-EVTS 
  '("button-down" 
    "button-up"
    "drag"
    "move"
    "enter"
    "leave"))

(define KEY-EVTS 
  '("left"
    "right"
    "up"
    "down"
    "start"
    "cancel"
    "clear"
    "shift"
    "rshift"
    "control"
    "rcontrol"
    "menu"
    "pause"
    "capital"
    "prior"
    "next"
    "end"
    "home"
    "escape"
    "select"
    "print"
    "execute"
    "snapshot"
    "insert"
    "help"
    "numpad0" "numpad1" "numpad2" "numpad3" "numpad4" 
    "numpad5" "numpad6" "numpad7" "numpad8" "numpad9" 
    "numpad-enter" "multiply" "add" "separator" "subtract" "decimal" "divide"
    "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12" "f13" 
    "f14" "f15" "f16" "f17" "f18" "f19" "f20" "f21" "f22" "f23" "f24"
    "numlock"
    "scroll"
    "wheel-up"
    "wheel-down"
    "wheel-left"
    "wheel-right"
    ))

(define-syntax (big-bang stx)
  (define world0 "expects an expression for the initial world and at least one clause")
  (syntax-case stx ()
    [(big-bang) (raise-syntax-error #f world0 stx)]
    [(big-bang w clause ...)
     (let* ([rec? #'#f]
            [->rec? 
             (lambda (kw E)
               (when (free-identifier=? kw #'record?)
                 (syntax-case E ()
                   [(V) (set! rec? #'V)]
                   [_ (err '#'record? stx)])))]
            [args 
             (->args 'big-bang stx #'w #'(clause ...) WldSpec ->rec?)]
            [dom (syntax->list #'(clause ...))])
       (cond
         [(and (not (contains-clause? #'to-draw dom)) (not (contains-clause? #'on-draw dom)))
          (raise-syntax-error #f "missing [to-draw renderer] or [on-draw renderer] clause" stx)]
         [else
          (syntax-property 
           (stepper-syntax-property
            #`(run-it ((new-world (if #,rec? aworld% world%)) w #,@args))
            'stepper-skip-completely #t)
           'disappeared-use (map (lambda (x) (car (syntax->list x))) dom))]))]))

(define-keywords Pad1Specs '() _init-not-needed
  [up    DEFAULT #'#f (function-with-arity 1)]
  [down  DEFAULT #'#f (function-with-arity 1)]
  [left  DEFAULT #'#f (function-with-arity 1)]
  [right DEFAULT #'#f (function-with-arity 1)]
  [space DEFAULT #'#f (function-with-arity 1)]
  [shift DEFAULT #'#f (function-with-arity 1)])

(define-syntax (pad-handler stx)
  (syntax-case stx ()
    [(pad1 clause ...)
     (let* ([args (->args 'pad-one-player stx #'w #'(clause ...) Pad1Specs void)]
            ; [_ (displayln args)]
            [keys (map (lambda (x) 
                         (syntax-case x () 
                           [(proc> (quote s) _f _d) (symbol->string (syntax-e #'s))]
                           [else "not present"]))
                       (filter values args))]
            [doms (map (lambda (x) (car (syntax->list x))) (syntax->list #'(clause ...)))])
       (syntax-property 
        (stepper-syntax-property
         #`(produce-handler '#,keys (list #,@args))
         'stepper-skip-completely #t)
        'disappeared-use doms))]))

(define (produce-handler keys args)
  (define (check k one other)
    (or (and (string=? k one) (not (member other keys)))
        (and (string=? k other) (not (member one keys)))))
  (define quasi-methods
    (for/fold ((m '())) ((k keys) (a args))
      (cond
        [(check k "w"     "up") (list* (cons "w" a) (cons "up" a) m)]
        [(check k "s"     "down") (list* (cons "s" a) (cons "down" a) m)]
        [(check k "a"     "left") (list* (cons "a" a) (cons "left" a) m)]
        [(check k "d"     "right") (list* (cons "d" a) (cons "right" a) m)]
        [(check k "shift" "rshift") (list* (cons "shift" a) (cons "rshift" a) m)]
        [else (cons (cons "space" a) m)])))
  (define quasi-object (make-immutable-hash quasi-methods))
  (define (the-handler* world key-event)
    ((hash-ref quasi-object key-event (lambda () values)) world))
  the-handler*)

(define (run-simulation f)
  (check-proc 'run-simulation f 1 "first" "one argument")
  (big-bang 0 (on-draw f) (on-tick add1)))

(define animate run-simulation)

(define (run-movie r m*)
  (check-arg 'run-movie (positive? r) "positive number" "first" r)
  (check-arg 'run-movie (list? m*) "list of images" "second" m*)
  (for-each (lambda (m) (check-image 'run-movie m "first" "list of images")) m*)
  (let* ([fst (car m*)]
         [wdt (image-width fst)]
         [hgt (image-height fst)])
    (big-bang 
      m* 
      (on-tick rest r)
      (on-draw (lambda (m) (if (empty? m) (text "The End" 22 'red) (first m))))
      (stop-when empty?))))

(define (mouse-event? a) (and (string? a) (pair? (member a MOUSE-EVTS))))

(define (mouse=? k m)
  (check-arg 'mouse=? (mouse-event? k) 'MouseEvent "first" k)
  (check-arg 'mouse=? (mouse-event? m) 'MouseEvent "second" m)
  (string=? k m))

(define (key-event? k) 
  (and (string? k) (or (= (string-length k) 1) (pair? (member k KEY-EVTS)))))

(define (key=? k m)
  (check-arg 'key=? (key-event? k) 'KEY-EVTS "first" k)
  (check-arg 'key=? (key-event? m) 'KEY-EVTS "second" m)
  (string=? k m))

(define LOCALHOST "127.0.0.1")

;                                                          
;                                                          
;                                                          
;   ;   ;           ;                                      
;   ;   ;           ;                                      
;   ;   ;                                                  
;   ;   ;  ;;;;     ;    ;   ;   ;;;   ; ;;    ;;;    ;;;  
;   ;   ;  ;   ;    ;    ;   ;  ;   ;  ;;  ;  ;   ;  ;   ; 
;   ;   ;  ;   ;    ;     ; ;   ;;;;;  ;   ;   ;;;   ;;;;; 
;   ;   ;  ;   ;    ;     ; ;   ;      ;          ;  ;     
;   ;   ;  ;   ;    ;      ;    ;   ;  ;      ;   ;  ;   ; 
;    ;;;   ;   ;    ;      ;     ;;;   ;       ;;;    ;;;  
;                                                          
;                                                          
;                                                          

(provide-primitives
 ;; type World 
 iworld?    ;; Any -> Boolean 
 iworld=?   ;; World World -> Boolean 
 iworld-name ;; World -> Symbol 
 ;; type Bundle = (make-bundle [Listof World] Universe [Listof Mail]) 
 ;; type Mail = (make-mail World S-expression)
 make-bundle ;; [Listof World] Universe [Listof Mail] -> Bundle 
 bundle?     ;; is this a bundle? 
 make-mail   ;; World S-expression -> Mail 
 mail?       ;; is this a real mail? 
 )

(provide 
 iworld1    ;; sample worlds 
 iworld2
 iworld3
 universe    ;; <syntax> : see below 
 )

(define-syntax (universe stx)
  (syntax-case stx ()
    [(universe) (raise-syntax-error #f "expects an expression for the initial world" stx)]
    [(universe u)
     (raise-syntax-error #f "expects at least an on-new and an on-msg clause after the initial world"
                         stx)]
    [(universe u bind ...)
     (let* ([args (->args 'universe stx #'u #'(bind ...) UniSpec void)]
            [dom (syntax->list #'(bind ...))])
       (cond
         [(not (contains-clause? #'on-new dom))
          (raise-syntax-error #f "expects a on-new clause, but found none" stx)]
         [(not (contains-clause? #'on-msg dom))
          (raise-syntax-error #f "expects a on-msg clause, but found none" stx)]
         [else ; (and (memq #'on-new dom) (memq #'on-msg dom))
          (syntax-property 
           #`(run-it ((new-universe universe%) u #,@args))
           'disappeared-use (map (lambda (x) (car (syntax->list x))) dom))]))]))

;                                          
;                                          
;                                          
;      ;               ;;;                 
;                     ;                    
;    ;;;    ;; ;;   ;;;;;;  ;; ;;;   ;;;;  
;      ;     ;;  ;    ;      ;;     ;    ; 
;      ;     ;   ;    ;      ;       ;;;;; 
;      ;     ;   ;    ;      ;      ;    ; 
;      ;     ;   ;    ;      ;      ;   ;; 
;    ;;;;;  ;;; ;;; ;;;;;;  ;;;;;    ;;; ;;
;                                          
;                                          
;                                          
;                                          

;; (-> Object) -> Any
;; Launches a big-bang window, unless it's within a check-big-bang form.
(define (run-it o)
  ; If this is called within a check-big-bang form,
  ; don't launch a window; call the send-world-obj
  ; continuation instead
  (define send-world-obj
    (continuation-mark-set-first
     (current-continuation-marks)
     check-big-bang-continuation-key
     #f))
  (cond [send-world-obj
         (parameterize ([big-bang-launches-window? #false])
           (send-world-obj (o)))]
        [else
         ; Otherwise launch the window and return the last state.
         (send (o) last)]))

#;
(define (run-it o)
  (define esp (make-eventspace))
  (define thd (eventspace-handler-thread esp))
  (with-handlers ((exn:break? (lambda (x) (break-thread thd))))
    (define obj:ch (make-channel))
    (parameterize ([current-eventspace esp])
      (queue-callback (lambda () (channel-put obj:ch (o)))))
    (send (channel-get obj:ch) last)))

;; ---------------------------------------------------------------------------------------------------

;; check-big-bang and check-big-bang*

;; Example usage:
;; (check-big-bang (main (make-posn 0 0))
;;   [(make-tick)        (make-posn 1 1)]
;;   [(make-key "right") (make-posn 6 1)]
;;   [(make-tick)        (make-posn 7 2)]
;;   [(make-key "left")  (make-posn 2 2)]
;;   [(make-key "down")  (make-posn 2 7)]
;;   [(make-tick)        (make-posn 3 8)]
;;   [(make-mouse 100 50 "button-down") (make-posn 100 50)]
;;   [(make-tick)        (make-posn 101 51)])

(begin-for-syntax
  (define-syntax-class check-big-bang-clause
    [pattern (~and stx [event:expr expected-state:expr])
             #:with norm
             (syntax/loc #'stx (make-event-expect event expected-state))]
    [pattern (~and stx [event:expr expected-state:expr expected-msg:expr])
             #:with norm
             (syntax/loc #'stx (make-event-expect-package event expected-state expected-msg))]))

(define-syntax check-big-bang
  (lambda (stx)
    (syntax-parse stx
      [(check-big-bang big-bang-expr:expr
                       clause:check-big-bang-clause
                       ...)
       (syntax/loc stx
         (check-big-bang* big-bang-expr
                          (list clause.norm
                                ...)))])))

(define-syntax check-big-bang*
  (lambda (stx)
    (syntax-parse stx
      [(check-big-bang* big-bang-expr:expr
                        event-expects-expr:expr)
       (check-expect-maker
        stx
        #'check-big-bang-function
        #'big-bang-expr
        (list #'event-expects-expr)
        'comes-from-check-big-bang)])))

;; An (EventExpectof WorldState) is one of:
;;  - (make-event-expect BigBangEvent WorldState)
;;  - (make-event-expect-package BigBangEvent WorldState S-Exp)
(struct event-expect (event world) #:transparent)
(struct event-expect-package (event world sent-msg) #:transparent)

;; event-expectation? : Any -> Boolean
(define (event-expectation? v)
  (or (event-expect? v) (event-expect-package? v)))

;; make-event-expect : BigBangEvent WorldState -> (EventExpectof WorldState)
(define (make-event-expect event world)
  (check-arg 'make-event-expect (check-big-bang-event? event) "check-big-bang event"
             "first" event)
  (event-expect event world))

;; make-event-expect-package : BigBangEvent WorldState S-Exp -> (EventExpectof WorldState)
(define (make-event-expect-package event world sent-msg)
  (check-arg 'make-event-expect-package (check-big-bang-event? event) "check-big-bang event"
             "first" event)
  (check-arg 'make-event-expect-package (sexp? sent-msg) "S-expression"
             "third" sent-msg)
  (event-expect-package event world sent-msg))

;; A BigBangEvent is one of:
;;  - (make-tick)
;;  - (make-key KeyEvent)
;;  - (make-pad PadEvent)
;;  - (make-release KeyEvent)
;;  - (make-mouse Integer Integer MouseEvent)
;;  - (make-receive S-Exp)
(struct tick () #:transparent #:extra-constructor-name make-tick)
(struct key (event) #:transparent)
(struct pad (event) #:transparent)
(struct release (event) #:transparent)
(struct mouse (x y event) #:transparent)
(struct receive (s-exp) #:transparent)

;; check-big-bang-event? : Any -> Boolean
(define (check-big-bang-event? v)
  (or (tick? v)
      (key? v)
      (pad? v)
      (release? v)
      (mouse? v)
      (receive? v)))

;; make-key : KeyEvent -> BigBangEvent
(define (make-key k)
  (check-arg 'make-key (key-event? k) "KeyEvent" "first" k)
  (key k))

;; make-pad : PadEvent -> BigBangEvent
(define (make-pad k)
  (check-arg 'make-pad (pad-event? k) "PadEvent" "first" k)
  (pad k))

;; make-release : KeyEvent -> BigBangEvent
(define (make-release k)
  (check-arg 'make-release (key-event? k) "KeyEvent" "first" k)
  (release k))

;; make-mouse : Integer Integer MouseEvent -> BigBangEvent
(define (make-mouse x y m)
  (check-arg 'make-mouse (integer? x) "Integer" "first" x)
  (check-arg 'make-mouse (integer? y) "Integer" "second" y)
  (check-arg 'make-mouse (mouse-event? m) "MouseEvent" "third" m)
  (mouse x y m))

;; make-receive : S-Exp -> BigBangEvent
(define (make-receive msg)
  (check-arg 'make-receive (sexp? msg) "S-expression" "first" msg)
  (make-receive msg))

;; check-big-bang-function : (-> Any) (Listof EventExpect) Srcloc Test-Engine -> Boolean
(define (check-big-bang-function big-bang-thunk event-expects src test-engine)
  (define world-obj
    (let/cc send-world-obj
      (with-continuation-mark check-big-bang-continuation-key send-world-obj
        (big-bang-thunk))))
  ; check that world-obj is an (Instance world%)
  (check-arg 'check-big-bang
             (and (object? world-obj) (is-a? world-obj world%))
             "big-bang expression"
             "first"
             world-obj)
  ; make sure that the timer doesn't add extra tick events
  (send (get-field timer world-obj) stop)
  ; check that event-expects is a (Listof EventExpect)
  (check-arg 'check-big-bang*
             (and (list? event-expects) (andmap event-expectation? event-expects))
             "list of event-expects"
             "second"
             event-expects)
  ; announce that it's running a test
  (define test-info (send test-engine get-info))
  (send test-info add-check)
  ; the success and failure functions
  ; -> Boolean
  (define (success)
    #true)
  ; Any Any -> Boolean
  (define (failure world expected)
    (send test-info check-failed
          (make-unequal src (test-format) world expected)
          world
          #false)
    #false)
  (check-big-bang-loop world-obj event-expects success failure))

;; check-big-bang-loop :
;; (Instance world%) (Listof Event-Expect) (-> Boolean) (Any Any -> Boolean) -> Boolean
(define (check-big-bang-loop world-obj event-expects success failure)
  (match event-expects
    ['()
     (success)]
    [(cons expectation rest)
     (check-big-bang-expect world-obj expectation rest success failure)]))

;; check-big-bang-expect :
;; (Instance world%) Event-Expect (Listof Event-Expect) (-> Boolean) (Any Any -> Boolean)
;; ->
;; Boolean
(define (check-big-bang-expect world-obj expectation rest success failure)
  (match expectation
    [(event-expect event expected)
     (define out (set-big-bang-package-output-string! world-obj))
     (send-big-bang-event! world-obj event)
     (yield 'wait)
     (define world (get-current-world world-obj))
     (cond
       [(not (equal? world expected))
        (failure world expected)]
       [(not (string=? (get-output-string out) ""))
        (failure "" (get-output-string out))]
       [else
        (check-big-bang-loop world-obj rest success failure)])]
    [(event-expect-package event expected expected-sent-msg)
     (define-values [in out]
       (set-big-bang-package-output-pipe! world-obj))
     (send-big-bang-event! world-obj event)
     (yield 'wait)
     (close-output-port out)
     (define world (get-current-world world-obj))
     (define sent-msg (read in))
     (close-input-port in)
     (cond
       [(not (equal? world expected))
        (failure world expected)]
       [(not (equal? sent-msg expected-sent-msg))
        (failure sent-msg expected-sent-msg)]
       [else
        (check-big-bang-loop world-obj rest success failure)])]))

;; get-current-world : (Instance world%) -> WorldState
(define (get-current-world world-obj)
  (send (get-field world world-obj) get))

;; send-big-bang-event! : (Instance world%) BigBangEvent -> Void
(define (send-big-bang-event! world-obj event)
  (match event
    [(tick) (send world-obj ptock)]
    [(key key-event) (send world-obj pkey key-event)]
    [(pad pad-event) (send world-obj ppad pad-event)]
    [(release key-event) (send world-obj prelease key-event)]
    [(mouse x y mouse-event) (send world-obj pmouse x y mouse-event)]
    [(receive message) (send world-obj prec message)]
    ))

;; set-big-bang-package-output-string! : (Instance world%) -> String-Output-Port
(define (set-big-bang-package-output-string! world-obj)
  (define out (open-output-string 'sent-messages))
  (set-field! *out* world-obj out)
  out)


;; set-big-bang-package-output-pipe! : (Instance world%) -> (values Input-Port Output-Por)
(define (set-big-bang-package-output-pipe! world-obj)
  (define-values [in out] (make-pipe))
  (set-field! *out* world-obj out)
  (values in out))

