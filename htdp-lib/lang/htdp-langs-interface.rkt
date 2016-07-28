#lang racket/base
(require racket/class)
(provide htdp-language<%>)
      
(define htdp-language<%>
  (interface ()
    get-module
    get-language-position
    get-sharing-printing
    get-abbreviate-cons-as-list
    get-allow-sharing?
    get-use-function-output-syntax?
    get-accept-quasiquote?
    get-read-accept-dot))