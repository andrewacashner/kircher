#! /usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

(use-modules
  (srfi srfi-1)
  (kircher arca)
  (kircher lectio)
  (kircher cogito))

(define main
  (lambda (args)
    "Usage: main.scm <INFILE> <OUTFILE>"
    (let* ([infile  (first (cdr args))]
           [outfile (second (cdr args))]
           [text    (make-text infile)]
           [music   (make-music text)]
           [cmd     (format #f "xmllint --format --output ~a ~a" 
                            outfile outfile)])
      (begin 
        (call-with-output-file 
          outfile 
          (lambda (port) (write music port)))
        (system cmd)))))

