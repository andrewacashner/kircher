; scribo-ly.scm
; Write output in Lilypond using object methods

(define-method 
  (lily o)
  (identity o))

(define-method
  (lily (o <music:unit>))
  (map lily (element o)))



(define-method
  (ly-oct (o <note>))
  (let ([oct (oct o)])
    (case oct
      ((0) ",,,")
      ((1) ",,")
      ((2) ",")
      ((3) "")
      ((4) "'")
      ((5) "''")
      ((6) "'''")
      (else (throw "Bad-ly-oct-value" oct)))))

(define-method
  (ly-accid (o <note>))
  (let ([accid (accid o)]
        [type (accid-type o)])
    (cond [(eq? accid-type 'ficta)
           (cond [(eq? accid 'flat)     "\\fl"]
                 [(eq? accid 'sharp)    "\\sh"]
                 [(eq? accid 'natural)  "\\na"])]
          [else
           (cond [(eq? accid 'flat)     "es"]
                 [(eq? accid 'sharp)    "is"]
                 [(eq? accid 'natural)  ""])])))

(define-method
  (ly-dots (o <note>))
  (let loop ([n (dots o)] [ls '()])
    (if (= 0 n)
        (reverse-list->string ls)
        (loop (1- n) (cons #\. ls)))))

(define-method
  (lily (o <note>))
  (let ([pname  (pname o)]
        [accid  (ly-accid o)] 
        [oct    (ly-oct o)]
        [dur    (dur o)]
        [dots   (ly-dots o)])
    (if (eq? (accid-type o) 'ficta)
        (format #f "~a~a~a~a~a" pname oct dur dots accid) 
        (format #f "~a~a~a~a~a" pname accid oct dur dots))))

(define-method
  (lily (o <rest>))
  (let ([dur  (dur o)]
        [dots (ly-dots o)])
    (format #f "r~a~a" dur dots)))

(define-method 
  (lily (o <barLine>)) 
  "|\n")

(define ly-meter
  (lambda (meter)
    (cond
      [(eq? meter 'duple)           "\\time 4/2"]
      [(eq? meter 'triple-minor)    "\\time 3/2"]
      [(eq? meter 'triple-major)    "\\time 3/1"]
      [else (throw "Bad-ly-meter" meter)])))

(define-method
  (ly-clef (o <voice>))
  (case (n o)
    ((1 2) "treble")
    ((3) "treble_8")
    ((4) "bass")))


(define-method
  (lily (o <voice>))
  (let* ([notes (map lily (element o))]
         [music (string-join notes " ")]
         [clef (ly-clef o)])
    (format 
      #f 
      "
\\new Staff
  <<
    \\new Voice {
      \\clef \"~a\"
      ~a
    }
  >>
" 
     clef music)))

(define-method
  (lily (o <voice>) meter)
  (let* ([notes (map lily (element o))]
         [music (string-join notes " ")]
         [clef (ly-clef o)]
         [meter-cmd (ly-meter meter)])
    (format 
      #f 
      "
\\new Staff
  <<
    \\new Voice {
      \\clef \"~a\"
      ~a
      ~a
    }
  >>
" 
      clef meter-cmd music)))

(define-method
  (lily (o <chorus>))
  (string-join (next-method)))

(define-method
  (lily (o <chorus>) meter)
  (let ([voices (element o)])
    (string-join (map (lambda (v) (lily v meter)) voices))))

(define-method 
  (lily (o <music:section>))
  (let ([meter (meter o)])
  (format 
    #f "
\\score {
<<
  \\new ChoirStaff
  <<
    ~a
  >>
>>
}
"
  (string-join (map (lambda (ch) (lily ch meter)) (element o))))))

(define-method
  (lily (o <music:composition>))
  (let ([music (next-method)])
  (format 
    #f 
    "
\\version \"2.19\"
\\include \"early-music.ly\"
\\include \"automatic-ties.ly\"
~a
"
(string-join (next-method)))))

