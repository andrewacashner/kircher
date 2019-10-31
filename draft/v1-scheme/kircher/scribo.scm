; scribo.scm
; Write output in Lilypond using object methods

(define-module
  (kircher scribo)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs enums)
  #:use-module (oop goops)
  #:use-module (kircher lectio)
  #:use-module (kircher arca)
  #:export (<note>
             <rest>
             <barLine>
             <voice>
             <chorus>
             <music:section>
             <music:composition>
             pnum
             pname
             oct
             pitch-dia
             pitch-chrom
             element
             pc-oct->pdia
             pdia->pc-oct))

(define rassoc
  (lambda (alist val pred?)
    "Return the key in ALIST for a given VAL, testing according to PRED"
    (let ([pair (find (lambda (pair) (pred? (cdr pair) val)) alist)])
      (car pair))))

(define last-less-eq
  (lambda (n ls)
    "Find the last element in LS where element <= N"
    (let loop ([ls ls])
      (if (null? (cdr ls))
          (first ls)
          (let ([this (first ls)]
                [next (second ls)])
            (if (and (>= n this)
                     (< n next))
                this
                (loop (cdr ls))))))))

(define alist-values
  (lambda (alist)
    "Given an alist, return a list containing just the values, 
    the cdr of each pair"
    (fold-right (lambda (this acc) (cons (cdr this) acc)) '() alist)))

(define pitch-ls '(c d e f g a b))
(define pitch-vec (list->vector pitch-ls))
(define pitch-symbol 
  (lambda (i)
    (vector-ref pitch-vec i)))
(define pitch-number (enum-set-indexer (make-enumeration pitch-ls)))

(define pc-oct->pdia
  (lambda (p o)
    "Convert diatonic pitchclass number and octave to absolute diatonic pitch
    number"
    (+ (* 7 o) p)))

(define pdia->pc-oct
  (lambda (p)
    "Convert absolute diatonic pitch number to pair where car is diatonic
    pitchclass number and cdr is octave"
    (let ([pnum (modulo p 7)]
          [oct  (floor (/ p 7))])
      (cons pnum oct))))

(define-class
  <music:unit> ()
  (data
    #:init-value '())
  (element 
    #:allocation #:virtual
    #:init-keyword #:element
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) (slot-set! o 'data ls))
    #:getter element))


(define-class 
  <note> ()
  (pnum 
    #:init-value 0
    #:init-keyword #:pnum
    #:accessor pnum)
  (oct 
    #:init-value 0
    #:init-keyword #:oct
    #:accessor oct)
  (accid 
    #:init-value 'natural
    #:init-keyword #:accid
    #:accessor accid)
  (accid-type 
    #:init-value 'default
    #:init-keyword #:accid-type
    #:accessor accid-type)
  (dur 
    #:init-value 0
    #:init-keyword #:dur
    #:accessor dur)
  (dots
    #:init-value 0
    #:init-keyword #:dots
    #:accessor dots)
  (syl
    #:init-value (make <syl>)
    #:init-keyword #:syl
    #:accessor syl)
  (syl-str 
    #:allocation #:virtual
    #:init-keyword #:syl-str
    #:slot-ref (lambda (o) (slot-ref (syl o) 'data))
    #:slot-set! (lambda (o s) 
                  (set! (syl o) 
                    (make <syl> #:data s))))

  ; Global lookup keys for class
  (accid-names
    #:allocation #:class
    #:getter accid-names
    #:init-value '((natural . #\n)
                   (flat    . #\f)
                   (sharp   . #\s)))
  (chrom-index
    #:allocation #:class
    #:getter chrom-index
    #:init-value '((0 . 0)
                   (1 . 2)
                   (2 . 4)
                   (3 . 5)
                   (4 . 7)
                   (5 . 9)
                   (6 . 11)))
  (accid-adjust
    #:allocation #:class
    #:getter accid-adjust
    #:init-value '((natural . 0)
                   (flat    . -1)
                   (sharp   . 1))) ; also double accidentals here and elsewhere

  ; Virtual slots
  (pname ; set pnum by letter symbol instead of number, access name 
    #:allocation #:virtual
    #:init-keyword #:pname
    #:accessor pname
    #:slot-ref (lambda (note) (pitch-symbol (pnum note)))
    #:slot-set! (lambda (note s)
                  (set! (pnum note)  ; TODO account for note 8?
                    (pitch-number s))))

  (accid-name ; set accid by char name instead of symbol, access char name
    #:allocation #:virtual
    #:init-keyword #:accid-name
    #:accessor accid-name
    #:slot-ref (lambda (note) 
                 (assq-ref (accid-names note) (accid note)))
    #:slot-set! (lambda (note c)
                  (set! (accid note) (rassoc (accid-names note) c char=?))))

  (pitch-dia ; set pnum and oct from pitch-dia, get pitch-dia from pnum & octave
    #:allocation #:virtual
    #:init-keyword #:pitch-dia
    #:accessor pitch-dia
    #:slot-ref (lambda (note) (pc-oct->pdia (pnum note) (oct note)))
    #:slot-set! (lambda (note pitch-dia)
                  (let ([pair (pdia->pc-oct pitch-dia)])
                    (begin
                      (set! (pnum note) (car pair))
                      (set! (oct note)  (cdr pair))))))
  (pitch-chrom ; set pnum, oct, and accid from chromatic pitch and v/v
    #:allocation #:virtual
    #:init-keyword #:pitch-chrom
    #:accessor pitch-chrom
    #:slot-ref (lambda (note)
                 (let ([octave (* 12 (oct note))]
                       [pnum-base (assq-ref (chrom-index note) (pnum note))]
                       [adj (assq-ref (accid-adjust note) (accid note))])
                   (+ octave pnum-base adj)))
    #:slot-set! (lambda (note pitch-chrom)
                  (let* ([octave (floor (/ pitch-chrom 12))]
                         [pnum-abs (modulo pitch-chrom 12)]
                         [steps (alist-values (chrom-index note))]
                         [pnum-base (last-less-eq pnum-abs steps)]
                         [adj (- pnum-abs pnum-base)]
                         [acc (rassoc (accid-adjust note) adj eq?)]
                         [pnum-dia (rassoc (chrom-index note) pnum-base eq?)])
                    (begin
                      (set! (pnum note) pnum-dia) 
                      (set! (oct note) octave)
                      (set! (accid note) acc))))))

(define-class <rest> (<note>))

(define-class <barLine> (<note>))

(define-class
  <voice> (<music:unit>)
  (n
    #:init-value '()
    #:init-keyword #:n
    #:getter n))

(define-class <chorus> (<music:unit>))

(define-class 
  <music:section> (<music:unit>)
  (meter
    #:init-value 'duple
    #:init-keyword #:meter
    #:getter meter)
  (keysig
    #:init-value '()
    #:init-keyword #:keysig
    #:getter keysig))

(define-class <music:composition> (<music:unit>))


(define-method 
  (lily o)
  (identity o))

(define-method
  (lily (o <music:unit>))
  (map lily (element o)))

(define-method
  (write (o <music:unit>) port)
  (display (lily o) port))


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
" (string-join (map (lambda (ch) (lily ch meter)) (element o))))))

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
" (string-join (next-method)))))

