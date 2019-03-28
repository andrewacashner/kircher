(use-modules
  (kircher arca)
  (kircher lectio)
  (kircher cogito))

(define main
  (lambda (args)
    (let* ([datafile    "data/arca.xml"]
           [textfile    (cadr args)]
           [arca        (make-arca datafile)]
           [text        (make-text textfile)]
           [music       (make-music arca text)])
      (display music))))

