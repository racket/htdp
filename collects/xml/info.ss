(lambda (sym fail)
  (let* ([sig "xmls.ss"]
         [signatures (list sig)])
    (case sym
      [(name) "XML"]
      [(compile-prefix) `(require-library ,sig "xml")]
      [(compile-omit-files) signatures]
      [(compile-elaboration-zos) signatures]
      [(help-desk-message) (format "Mz/Mr: ~s" `(require-library "xml.ss" "xml"))]
      [(blurb)
       `("The XML colleciton provides functions for reading, writing, and manipulating XML documents.")]
      ;[(compile-subcollections) (list (list "xml" "xt3d"))]
      [else (fail)])))
