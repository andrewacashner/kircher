#! /usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

(use-modules
  (srfi srfi-1)
  (kircher arca)
  (kircher lectio)
  (kircher scribo)
  (kircher musarithmetic)
  (kircher cogito))

(define main
  (lambda (args)
    "Usage: main.scm <INFILE> <OUTFILE>"
    (let* ([infile  (first (cdr args))]
           [outfile (second (cdr args))]
           [outdir  (dirname outfile)]
           [text    (make-text infile)]
           [music   (make-music text)]
           [cmd     (format #f "xmllint --format --output ~a ~a" 
                            outfile outfile)])
      (begin 
        (unless (access? outdir W_OK) (mkdir outdir))
        (call-with-output-file outfile (lambda (port) (write music port)))
        (system cmd)))))

