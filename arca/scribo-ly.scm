; scribo.ly -- Functions to produce lilypond output;
; not needed if output is mei and we can use lirio xsl to convert

(define-method
  (ly (note <note>))
  (format #f "~c~a~a~d~a~a"
          (pname note)
          (ly-accid note)
          (ly-oct note)
          (dur note)
          (ly-dots note)
          (ly-accid-ficta note)))

(define-method 
  (ly-accid (note <note>))
  (let ([a (accid note)])
  (cond
    [(eq? a 'flat) "es"]
    [(eq? a 'sharp) "is"]
    [else ""])))

(define-method
  (ly-oct (note <note>))
  (let* ([oct (oct note)]
         [imax (abs (- oct 4))])
    (let loop ([i 0] [ls '()])
      (if (> i imax)
          (list->string ls)
          (let ([node (cond [(> oct 3) (cons #\' ls)]
                            [(< oct 3) (cons #\, ls)]
                            [else ls])])
            (loop (1+ i) node))))))

(define-method
  (ly-dots (note <note>))
  (let ([d (dots note)])
    (if (> d 0)
        (let loop ([i 0] [ls '()])
          (if (>= i d)
              (list->string ls)
              (loop (1+ i) (cons #\. ls))))
        "")))

(define-method
  (ly-accid-ficta (note <note>))
  (let ([alist '((natural . "\\na")
                 (flat . "\\fl")
                 (sharp . "\\sh"))]
        [type (accid-type note)])
    (if (eq? type 'ficta)
        (assq-ref alist (accid note))
        "")))

(define-method
  (ly (rest <rest>))
  (format #f "~c~d~a"
          #\r ; add full-bar type
          (dur rest)
          (ly-dots rest)))

(define-method
  (ly (voice <voice>)) 
  (format #f "\\new Voice = \"~a\" {\n~a\n}\n"
          (id voice) 
          (string-join (map ly (notes voice)) " ")))


(define-method
  (ly (syl <syl>))
  (str syl))

(define-method 
  (ly (word <word>))
  (let ([str-ls (map str (syl-ls word))])
    (string-join str-ls " -- ")))

(define-method
  (ly (phrase <phrase>))
  (string-join (map ly (word-ls phrase)) " "))

(define arca->ly
  (lambda (text)
    
    (define sentence->ly
      (lambda (s)
        (ly (car s))))

    (string-join (map sentence->ly text))))

(define file->ly
  (lambda (infile)
    (arca->ly (file->arca infile))))

