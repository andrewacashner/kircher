(define-module
  (kircher scribo)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs enums)
  #:use-module (oop goops)
  #:use-module (sxml simple)
  #:use-module (kircher sxml)
  #:use-module (kircher lectio)
  #:use-module (kircher arca)
  #:export (<note>
             <rest>
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
#|
(use-modules 
  (srfi srfi-1)
  (rnrs enums)
  (oop goops)
  (sxml simple)
  (kircher sxml)
  (kircher lectio)
  (kircher arca))
|#

;; {{{1 UTILITIES
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
;; }}}1

;; {{{1 CLASSES and METHODS
;; {{{2 music:unit 
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

(define-method
  (sxml o)
  (identity o))

(define-method 
  (sxml (o <music:unit>))
  (map sxml (element o)))

(define-method
  (write (o <music:unit>) port)
  (sxml->xml (sxml o) port))

;; {{{2 <note>
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
;; {{{3 <note> methods
(define-method
  (sxml-accid-char (note <note>))
  (let ([alist '((natural   . #\n)
                 (flat      . #\f)
                 (sharp     . #\s))]) 
    (assq-ref alist (accid note))))

(define-method
  (sxml-accid (note <note>))
  (let* ([name  (accid note)]
         [type  (accid-type note)]
         [char  (sxml-accid-char note)]
         [accid (cond [(eq? type 'default) 
                       (if (eq? name 'natural)
                           '()
                           (sxml-node 'accid char))]

                      [(eq? type 'ficta)
                       (list (sxml-node 'accid char) 
                             (sxml-node 'func "ficta"))]

                      [(eq? type 'signature)
                       (sxml-node 'accid.ges char)]

                      [else (throw "Bad-accid-value" name type char)])]

         [attr  (sxml-node '@ accid)])
    (sxml-node 'accid attr)))

(define-method
  (sxml-dots (note <note>))
  (let ([dot-val (dots note)])
    (nullify-match dot-val = 0)))

(define-method
  (sxml (note <note>))
  (let* ([pname     (sxml-node 'pname   (pname note))]
         [oct       (sxml-node 'oct     (oct note))]
         [dur       (sxml-node 'dur     (dur note))]
         [dots      (sxml-node 'dots    (sxml-dots note))]
         [attr      (sxml-node '@       pname oct dur dots)]
         [syl       (sxml-node 'verse   (sxml (syl note)))] 
         [accid     (sxml-accid note)]) 
    (sxml-node 'note attr accid syl)))

;(define-method
;  (write (o <note>) port)
;  (sxml->xml (sxml o) port))

;; }}}3

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

;; }}}2

;; {{{2 <rest>
(define-class <rest> (<note>))

(define-method
  (sxml (rest <rest>))
  (let* ([dur       (sxml-node 'dur (dur rest))]
         [dots      (sxml-dots rest)]
         [attr      (sxml-node '@ dur dots)])
    (sxml-node 'rest attr)))
;; }}}2

;; {{{2 <voice> and <chorus>
(define-class
  <voice> (<music:unit>)
  (n
    #:init-value '()
    #:init-keyword #:n
    #:getter n))

(define-method
  (note-ref (voice <voice>) (i <number>))
  (list-ref (element voice) i))

(define mensuration
  (lambda (meter)
    (let ([tempora 
            '((duple        . (mensur (@ (tempus 2) (prolatio 2))))
              (triple-major . (mensur (@ (tempus 2) (prolatio 2) 
                                         (slash 1) (num 3))))
              (triple-minor . (mensur (@ (tempus 2) (prolatio 2) 
                                         (num 3)))))])
      (assq-ref tempora meter))))

(define-method
  (sxml (o <voice>))
  (let* ([n         (sxml-node 'n (n o))]
         [attr      (sxml-node '@ n)]
         [layer     (sxml-node 'layer attr (next-method))])
    (sxml-node 'staff attr layer)))

(define-method
  (sxml (o <voice>) meter)
  (let* ([n         (sxml-node 'n (n o))]
         [attr      (sxml-node '@ n)]
         [mensur    (mensuration meter)]
         [layer     (sxml-node 'layer attr mensur
                               (map sxml (element o)))])
    (sxml-node 'staff attr layer)))

(define-class <chorus> (<music:unit>))

(define-method
  (sxml (o <chorus>))
  (sxml-node 'measure (next-method)))

(define-method
  (sxml (o <chorus>) meter)
  (let* ([voices    (element o)] 
         [ls        (map (lambda (v) (sxml v meter)) voices)])
    (sxml-node 'measure ls)))

;; }}}2

;; {{{2 music:section, :composition
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

(define +staff-grp+
  `(staffGrp 
     (@ (n "1") (symbol "bracket"))
     (staffDef (@ (n "1") (lines "5") (clef.line "2") (clef.shape "G")))
     (staffDef (@ (n "2") (lines "5") (clef.line "2") (clef.shape "G")))
     (staffDef (@ (n "3") (lines "5") (clef.line "2") (clef.shape "G") 
                  (clef.dis "8") (clef.dis.place "below")))
     (staffDef (@ (n "4") (lines "5") (clef.line "4") (clef.shape "F")))))

(define-method
  (sxml (o <music:section>))
  (let* ([meter     (meter o)]
         [keysig    (keysig o)]
         [key       (sxml-node 'key.sig keysig)]
         [attr      (sxml-node '@ key)]
         [scoreDef  (sxml-node 'scoreDef attr +staff-grp+)]

         [choruses  (element o)]
         [first     (sxml (car choruses) meter)]
         [rest      (map sxml (cdr choruses))])
    (sxml-node 'section scoreDef (list first rest))))

(define-class <music:composition> (<music:unit>))

(define-method
  (sxml (o <music:composition>))
  `(*TOP* 
     (*PI* xml "version=\"1.0\" encoding=\"utf-8\"") 
     (mei (@ (xmlns "https://www.music-encoding.org/ns/mei")) 
          (meiHead (fileDesc (title "ARCA"))) 
          (music (body (mdiv (score (scoreDef ,+staff-grp+)
                                    ,(next-method))))))))
; TODO add final barline @right="end" in last measure
;; }}}2



