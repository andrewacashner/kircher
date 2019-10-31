(use-modules
  (ice-9 popen))

(define read-xml-xinclude
  (lambda (infile)
    (let* ([text (call-with-input-file infile get-string-all)]
           ; Use xmllint to process xi:includes
           [xmllint-cmd (format #f "xmllint --xinclude ~a" infile)]
           [xmllint-port (open-input-pipe xmllint-cmd)]
           [xml (get-string-all xmllint-port)])
      (begin 
        (close-pipe xmllint-port) 
        xml))))


