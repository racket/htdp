(lambda (request failure)
  (case request
    [(name) "The Foot"]
    [(compile-prefix) '(begin (require-library "sig.ss" "stepper")
                              (require-library "drsig.ss" "drscheme"))]
    [(compile-omit-files) '("test.ss" "testr.ss" "sig.ss")]
    [else (failure)]))