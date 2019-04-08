;; vim: set foldmethod=marker :

;; cogito.scm
;; Andrew A. Cashner
;; 2019/03/13--04/05

;; improve mode adjustment, key signature, interval adjustment between notes and
;; voices

(define-module 
  (kircher cogito)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (sxml simple)
  #:use-module (kircher sxml)
  #:use-module (kircher lectio)
  #:use-module (kircher arca)
  #:export (make-music))


;(use-modules 
;  (srfi srfi-1)
;  (oop goops)
;  (sxml simple)
;  (kircher sxml)
;  (kircher lectio)
;  (kircher arca))

;; {{{1 UTILITIES
(define screen-value
  (lambda (given type allowed)
    "Check if GIVEN is in list ALLOWED; if not throw exception; if yes return
    GIVEN"
    (if (not (member given allowed)) 
        (throw (format #f "Bad ~a value" type) given)
        given)))

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
  (pitchnames
    #:allocation #:class
    #:getter pitchnames
    #:init-value "cdefgabc") ; TODO fix pitch # 8
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
    #:slot-ref (lambda (note) 
                 (string-ref (pitchnames note) (pnum note)))
    #:slot-set! (lambda (note c)
                    (set! (pnum note) 
                      (string-index (pitchnames note) c))))

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
    #:slot-ref (lambda (note) 
                 (+ (* 7 (oct note)) (pnum note)))
    #:slot-set! (lambda (note pitch-dia)
                  (begin
                    (set! (pnum note) (modulo pitch-dia 7))
                    (set! (oct note) (floor (/ pitch-dia 7))))))
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
         [layer     (sxml-node 'layer attr (next-method) '(barLine))])
    (sxml-node 'staff attr layer)))

(define-method
  (sxml (o <voice>) meter)
  (let* ([n         (sxml-node 'n (n o))]
         [attr      (sxml-node '@ n)]
         [mensur    (mensuration meter)]
         [layer     (sxml-node 'layer attr mensur
                               (map sxml (element o)) 
                               '(barLine))])
    (sxml-node 'staff attr layer)))

(define-class <chorus> (<music:unit>))

(define-method
  (sxml (o <chorus>))
  (sxml-node 'section (next-method)))

(define-method
  (sxml (o <chorus>) meter)
  (let ([voices (element o)])
  (sxml-node 'section (map (lambda (v) (sxml v meter)) voices))))

;; }}}2

;; {{{2 music:section, :composition
(define-class 
  <music:section> (<music:unit>)
  (meter
    #:init-value 'duple
    #:init-keyword #:meter
    #:getter meter))

(define +score-def+
  `(scoreDef
     (staffGrp 
       (@ (n "1") (symbol "bracket"))
       (staffDef (@ (n "1") (lines "5") (clef.line "2") (clef.shape "G")))
       (staffDef (@ (n "2") (lines "5") (clef.line "2") (clef.shape "G")))
       (staffDef (@ (n "3") (lines "5") (clef.line "2") (clef.shape "G") 
                    (clef.dis "8") (clef.dis.place "below")))
       (staffDef (@ (n "4") (lines "5") (clef.line "4") (clef.shape "F"))))))

(define-method
  (sxml (o <music:section>))
  (let* ([meter     (meter o)]
         [choruses  (element o)]
         [first     (sxml (car choruses) meter)]
         [rest      (map sxml (cdr choruses))])
    (sxml-node 'section (list first rest))))

; TODO + key

(define-class <music:composition> (<music:unit>))

(define-method
  (sxml (o <music:composition>))
  `(*TOP* 
     (*PI* xml "version=\"1.0\" encoding=\"utf-8\"") 
     (mei (@ (xmlns "https://www.music-encoding.org/ns/mei")) 
          (meiHead (fileDesc (title "ARCA"))) 
          (music (body (mdiv (score ,+score-def+
                                    ,(next-method))))))))
;; }}}2


;; }}}1

(include "musarithmetic.scm")

;; {{{1 Read text input, calculate and compose music using arca
(define-method
  (make-rest (o <rnode>))
  (make <rest> #:dur (get-dur o)))

(define-method 
  (make-note (syl <syl>) pnum (rnode <rnode>) mode range voice-id)
  "Given syllable object from input file, pitch number from arca,
  and rhythmic permutation from arca, create <note> object;
  adjust note for mode and put in correct range for given voice"
  (let* ([pnum (if (= pnum 8) 0 (1- pnum))] ; 0 index
         [note      (make <note> 
                          #:pnum  pnum
                          #:dur   (get-dur rnode)
                          #:dots  (get-dots rnode)
                          #:syl   syl)]
         [in-mode   (adjust-mode note mode)]
         [in-range  (adjust-range in-mode range voice-id)])
    in-range))
; TODO use pitch-dia to set pnum and octave at same time and avoid note 8
; problem

(define-method
  (phrase->syl (o <phrase>))
  (let* ([words (slot-ref o 'element)]
         [syllables (map (lambda (o) (slot-ref o 'element)) words)])
    (flatten syllables)))

(define-method
  (music-combine (o <phrase>) voice rperm mode range voice-id)
  "Given a phrase of text, a list of voice nums, and a list of <rnode> objects,
  combine the three elements to make a list of <note> or <rest> objects."
  (let loop ([sls (phrase->syl o)] [vls voice] [rls rperm] [new '()])
    (if (null? sls)
        (reverse new)
        (let ([s (car sls)] [v (car vls)] [r (car rls)])
          (if (rest? r)
              ; If rhythm is a rest, make a rest and add to list,
              ; move to next rhythm, but keep same syl and vnum
              (let ([rest (make-rest r)]) 
                (loop sls vls (cdr rls) (cons rest new)))
              ; Otherwise combine syl, vnum, and dur to make note, add to list
              (let ([note (make-note s v r mode range voice-id)])
                (loop (cdr sls) (cdr vls) (cdr rls) (cons note new))))))))


(define-method
  (phrase->music (o <phrase>) (arca <arca>) style range meter mode)
  "Make a list of music <note> objects setting PHRASE with music selected from
  ARCA according to the given parameters and properties of the phrase"
  (let* ([syl-count (syl-count o)]
         [len       (penult-len o)] ; only works for syntagma1
         [column    (get-column arca style syl-count len)]
         [vperm     (get-vperm column)] ; = list of lists of pitch nums
         [rperm     (get-rperm column meter)] ; = list of <rnode> objects
         [ls        (let ([ls (reverse vperm)])
                        (map (lambda (v)
                               (music-combine o v rperm mode 
                                              range (length (member v ls))))
                             ls))])
         (reverse ls)))

(define-method
  (sentence->music (o <sentence>) (arca <arca>) style range meter mode)
  (let loop ([ls (slot-ref o 'element)] [music '()])
    (if (null? ls)
        (let* ([phrases         (reverse music)]
               [sentence-groups (apply zip phrases)]
               [sentence        (map flatten sentence-groups)]
               [voices          (map (lambda (ls) 
                                       (make <voice> #:element ls))
                                     sentence)]
               [chorus          (make <chorus> #:element voices)]
               [chorus-num      (number-voices chorus)] 
               [adjusted        (set-octaves chorus-num)])
          adjusted)

        (let ([satz (phrase->music 
                      (car ls) arca style range meter mode)])
          (loop (cdr ls) (cons satz music))))))
 
(define-method
  (section->music (o <section>) (arca <arca>) style range)
  (let* ([meter         (slot-ref o 'meter)]
         [mood          (slot-ref o 'mood)]
         [mode          (select-mode mood)])
    (let loop ([ls (slot-ref o 'element)] [music '()])
      (if (null? ls)
          (make <music:section> #:meter meter #:element (reverse music))
          (let ([sentence (sentence->music 
                            (car ls) arca style range meter mode)])
           (loop (cdr ls) (cons sentence music)))))))




(define-method
  (make-music (o <text>))
  (let* ([arca       +arca+]
         [style      (select-style       (slot-ref o 'style))]
         [range      (select-range-type  (slot-ref o 'clefs))]
         [sections   (slot-ref o 'element)]
         [music      (map (lambda (ls) 
                            (section->music ls arca style range)) 
                          sections)])
    (make <music:composition> #:element music)))
; + title, header info

