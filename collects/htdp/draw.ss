#cs(module draw mzscheme
  (require (lib "big-draw.ss" "htdp")
           (lib "draw-sig.ss" "htdp")
           (lib "unitsig.ss"))

     (define-syntax draw 
       (syntax-rules (produce)
         [(_ stmt ... produce exp) (begin (and stmt ...) exp)]))
     
     (provide 
      draw ;; (draw <expression> ... produce <expression>)
      )

     (provide-signature-elements draw^))
