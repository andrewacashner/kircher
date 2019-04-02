;; vim: set foldmethod=marker :

;; cogito.scm
;; Andrew A. Cashner
;; 2019/03/13

; TODO write MEI output by section, sentence, and phrase
; link lyrics and music or put in separate sections
; write modify, in-mode, and correct algorithms

(use-modules 
  (srfi srfi-1)
  (oop goops)
  (sxml simple)
  (kircher lectio)
  (kircher arca))

;; TODO integrate syl to note creation with wordpos etc.
;; or do separately

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
          ls
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
    #:init-value 'none
    #:init-keyword #:syl
    #:accessor syl)

  ; Global lookup keys for class
  (pitchnames
    #:allocation #:class
    #:getter pitchnames
    #:init-value "cdefgabC") ; TODO fix pitch # 8
  (accid-names
    #:allocation #:class
    #:getter accid-names
    #:init-value '((natural . #\n)
                   (flat . #\f)
                   (sharp . #\s)))
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
                   (flat . -1)
                   (sharp . 1))) ; also double accidentals here and elsewhere

  ; Virtual slots
  (pname ; set pnum by letter symbol instead of number, access name 
    #:allocation #:virtual
    #:init-keyword #:pname
    #:accessor pname
    #:slot-ref (lambda (note) 
                 (string-ref (pitchnames note) (pnum note)))
    #:slot-set! (lambda (note sym)
                  (let* ([str (symbol->string sym)]
                         [c (string-ref str 0)])
                    (set! (pnum note) (string-index (pitchnames note) c)))))

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
                 (let ([octave (* 11 (oct note))]
                       [pnum-base (assq-ref (chrom-index note) (pnum note))]
                       [adj (assq-ref (accid-adjust note) (accid note))])
                   (+ octave pnum-base adj)))
    #:slot-set! (lambda (note pitch-chrom)
                  (let* ([octave (floor (/ pitch-chrom 11))]
                         [pnum-abs (modulo pitch-chrom 11)]
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
;; note arithmetic
;; not mutating given note
(define-method
  (arithmetic (fn <procedure>) (access <accessor>) (note <note>) (n <number>))
  "Return a new note whose pnum and oct have been set by 
  applying procedure FN to NOTE with arg N"
  (let ([new (deep-clone note)]
        [pitch (fn (access note) n)])
    (begin 
      (set! (access new) pitch)
      new)))

(define-method
  (inc (note <note>) (n <number>))
  "Increase pitch of NOTE by N diatonic steps; return new note"
  (arithmetic + pitch-dia note n))

(define-method
  (inc-chrom (note <note>) (n <number>))
  "Increase pitch of NOTE by N chromatic steps; return new note"
  (arithmetic + pitch-chrom note n))

;; YES mutating given note
(define-method
  (arithmetic! (fn <procedure>) (access <accessor>) (note <note>) (n <number>))
  "Return a new note whose pnum and oct have been set by 
  applying procedure FN to NOTE with arg N"
  (let ([pitch (fn (access note) n)])
    (set! (access note) pitch)
    note))

(define-method
  (inc! (note <note>) (n <number>))
  "Increase pitch of NOTE by N diatonic steps; mutate given note"
  (arithmetic! + pitch-dia note n))

(define-method
  (inc-chrom! (note <note>) (n <number>))
  "Increase pitch of NOTE by N chromatic steps; mutate given note"
  (arithmetic! + pitch-chrom note n))


; shift octaves
; copy
(define-method (8va (note <note>)) (inc note 7))
(define-method (8vb (note <note>)) (inc note -7))

;original
(define-method (8va! (note <note>)) (inc! note 7))
(define-method (8vb! (note <note>)) (inc! note -7))


; difference of two notes
(define-method
  (diff (note1 <note>) (note2 <note>))
  "Return integer difference between diatonic pitch of NOTE1 and NOTE2"
  (- (pitch-dia note1) (pitch-dia note2)))

(define-method
  (diff-chrom (note1 <note>) (note2 <note>)) 
  "Return integer difference between chromatic pitch of NOTE1 and NOTE2"
  (- (pitch-chrom note1) (pitch-chrom note2)))


; compare
(define-method
  (cmp (fn <procedure>) (note1 <note>) (note2 <note>))
  (let ([p1 (pitch-chrom note1)]
        [p2 (pitch-chrom note2)])
    (fn p1 p2)))

(define-method
  (note=? (note1 <note>) (note2 <note>))
  (cmp = note1 note2))

(define-method
  (note>? (note1 <note>) (note2 <note>))
  (cmp > note1 note2))

(define-method
  (note<? (note1 <note>) (note2 <note>))
  (cmp < note1 note2))

(define-method
  (note>=? (note1 <note>) (note2 <note>))
  (cmp >= note1 note2))

(define-method
  (note<=? (note1 <note>) (note2 <note>))
  (cmp <= note1 note2))

(define-method
  (tritone? (note1 <note>) (note2 <note>))
  (let* ([diff (abs (diff-chrom note1 note2))]
         [int (modulo diff 11)])
    (= int 6)))

; write
(define-method
  (sxml (note <note>))
  (let* ([pname      `(pname ,(pname note))]
         [oct        `(oct ,(oct note))]
         [dur        `(dur ,(dur note))]
         [dots       `(dots ,(dots note))]
         [syl        (sxml syl)]
         [attr       `(@ ,pname ,oct ,dur ,dots)]
         [accid      (sxml-accid note)])
    (if (null? accid)
        `(note ,attr) 
        `(note ,attr ,accid))))

(define-method
  (sxml-accid-char (note <note>))
  (let ([alist '((natural   . #\n)
                 (flat      . #\f)
                 (sharp     . #\s))]) 
    (assq-ref alist (accid note))))

(define-method
  (sxml-accid (note <note>))
  (let* ([name   (accid note)]
         [type   (accid-type note)]
         [char   (sxml-accid-char note)]

         [attr   (case (type)
                   ['(default)   (case (name) 
                                   ['(natural)  '()] 
                                   [else        'accid])]

                   ['(ficta)     'accid]
                   ['(signature) 'accid.ges])] 

         [func   (case (type) 
                   ['(ficta)    '(func "ficta")] 
                   [else        '()])])

         (if (null? attr)
             '()
             (if (null? func)
                 `(accid (@ (,attr ,char)))
                 `(accid (@ (,attr ,char) ,func))))))

(define-method
  (write (o <note>) port)
  (sxml->xml (sxml o) port))

;; }}}3
;; }}}2

;; {{{2 <rest>
(define-class <rest> (<note>))

(define-method
  (smei (rest <rest>))
  `(rest (@ (dur ,(dur rest))
            (dots ,(dots rest)))))
;; }}}2

;; {{{2 <voice>
(define-class
  <voice> (<list>)
  (id 
    #:init-value 'unset
    #:init-keyword #:id
    #:getter id)
  (name
    #:init-value ""
    #:init-keyword #:name
    #:getter name)
  (note-ls
    #:init-value '())
  (notes
    #:allocation #:virtual
    #:init-keyword #:notes
    #:accessor notes
    #:slot-ref (lambda (o) (slot-ref o 'note-ls))
    #:slot-set! (lambda (o ls)
                  (if (every (lambda (n) (is-a? n <note>)) ls)
                      (slot-set! o 'note-ls ls)
                      (throw 'invalid-notelist ls)))))

;; {{{3 <voice> methods
(define-generic append)
(define-method
  (append (voice <voice>) (note <note>))
  (let* ([ls (reverse (notes voice))]
         [new (reverse (cons note ls))])
    (set! (notes voice) new)))

(define-method
  (note-ref (voice <voice>) (i <number>))
  (list-ref (notes voice) i))


(define-method
  (smei (voice <voice>))
  `(staff (@ (id ,(id voice))
             (label ,(name voice)))
          (layer ,(map smei (notes voice)))))

(define-method
  (mei (voice <voice>))
  (sxml->xml (smei voice)))

(define-method
  (write (voice <voice>) port)
  (mei voice))


(define-method
  (test-range (voice <voice>) (extreme <note>) (test <procedure>))
  (any (lambda (note) (test note extreme)) (notes voice)))

(define-method
  (range-too-low? (voice <voice>) (extreme <note>))
  (test-range voice extreme note<?))

(define-method
  (range-too-high? (voice <voice>) (extreme <note>))
  (test-range voice extreme note>?))

(define-method
  (transpose! (voice <voice>) (interval <number>))
  (let* ([old (notes voice)]
         [new (map (lambda (note) (inc note interval)) old)])
    (begin 
      (set! (notes voice) new)
      voice)))

; TODO add chromatic equivalent
; add symbols for chromatic intervals (e.g. m6, a5)

(define octave-interval
  (lambda (n) (* 7 n)))

(define range-extreme
  (lambda (id type)
    (let* ([range 
             ; traditional clefs G2, C3, C4, F4
             '((soprano . ((low . (g . 5)) (high . (d . 5))))
               (alto    . ((low . (e . 4)) (high . (a . 4))))
               (tenor   . ((low . (c . 3)) (high . (f . 4))))
               (bass    . ((low . (f . 3)) (high . (b . 3)))))]
           [voice (assq-ref range id)]
           [pair (assq-ref voice type)]
           [pname (car pair)]
           [oct (cdr pair)])
      (make <note> #:pname pname #:oct oct))))
; put into voice object class?

(define-method
  (adjust-range-up! (voice <voice>))
  (if (range-too-low? voice (range-extreme (id voice) 'low)) 
      (begin 
        (transpose! voice (octave-interval 1))
        (adjust-range-up! voice)) 
      voice))

(define-method
  (adjust-range-down! (voice <voice>))
  (if (range-too-high? voice (range-extreme (id voice) 'high)) 
      (begin 
        (transpose! voice (octave-interval -1))
        (adjust-range-down! voice))
      voice))

(define-method
  (tritone? (voice1 <voice>) (voice2 <voice>))
  "Boolean: Are there any tritones between the two voices?"
  (let ([ls (zip (notes voice1) (notes voice2))])
    (any (lambda (node) (tritone? (first node) (second node))) ls)))
; better to report back the indexes of any tritones
;; }}}3
;; }}}2

;; {{{2 <chorus>
(define-class 
  <chorus> (<list>)
  (id 
    #:init-keyword #:id
    #:getter id
    #:init-value 'unset)
  (name 
    #:init-keyword #:name
    #:getter name
    #:init-value "")
  (voice-ls
    #:init-value '())
  (voices
    #:allocation #:virtual
    #:init-keyword #:voices
    #:accessor voices
    #:slot-ref (lambda (o) (slot-ref o 'voice-ls))
    #:slot-set! (lambda (o ls)
                  (if (every (lambda (n) (is-a? n <voice>)) ls)
                      (slot-set! o 'voice-ls ls)
                      (throw 'invalid-voicelist ls)))))

(define-method
  (smei (chorus <chorus>))
  `(score (staffGrp ,(map smei (voices chorus)))))

(define-method
  (mei (chorus <chorus>))
  (sxml->xml (smei chorus)))

(define-method
  (write (chorus <chorus>) port)
  (mei chorus))
;; }}}2
;; }}}1

;; {{{1 Read text input, calculate and compose music using arca
(define-method
  (make-rest (o <rnode>))
  (make <rest> #:dur (get-dur o)))

(define-method 
  (make-note (syl <syl>) voice-num (rnode <rnode>))
      (make <note> 
            #:pnum  (1- (string->number voice-num)) ; convert to 0-index
            #:dur   (get-dur rnode)
            #:dots  (get-dots rnode)
            #:syl   syl))

(define-method
  (music-combine (phrase <phrase>) voice rperm)
  "Given a phrase of text, a list of voice nums, and a list of <rnode> objects,
  combine the three elements to make a list of <note> or <rest> objects"
    (let loop ([sls (phrase->syl phrase)] [vls voice] [rls rperm] [new '()])
      (if (null? sls)
        (reverse new)
        (let ([s (car sls)] [v (car vls)] [r (car rls)])
          (if (rest? r)
              ; If rhythm is a rest, make a rest and add to list,
              ; move to next rhythm, but keep same syl and vnum
              (let ([rest (make-rest r)]) 
                (loop sls vls (cdr rls) (cons rest new)))
              ; Otherwise combine syl, vnum, and dur to make note and add to
              ; list
              (let ([note (make-note s v r)])
                (loop (cdr sls) (cdr vls) (cdr rls) (cons note new))))))))

(define mode-convert
  (lambda (ls mode)
    "DUMMY"
    (identity ls)))

(define set-range 
  (lambda (vperm range-sym)
  "DUMMY"
  (identity vperm)))

(define-method
  (phrase->music (phrase <phrase>) (arca <arca>) style range meter mode)
  "Make a list of music <note> objects setting PHRASE with music selected from
  ARCA according to the given parameters and properties of the phrase"
  (let* ([syl-count (syl-count phrase)]
         [len       (penult-len phrase)] ; only works for syntagma1
         [column    (get-column arca style syl-count len)]
         [vperm     (get-vperm column)] ; = list of lists of pitch nums
         [vmode     (mode-convert vperm mode)]
         [voices    (set-range vmode range)]
         [rperm     (get-rperm column meter)]) ; = list of <rnode> objects
    (map (lambda (voice) (music-combine phrase voice rperm)) voices)))

(define-method
  (sentence->music (sent <sentence>) (arca <arca>) style range meter mode)
  (let ([phrases (slot-ref sent 'element)])
    (map (lambda (phrase) 
           (phrase->music phrase arca style range meter mode)) 
         phrases)))

(define select-meter 
  (lambda (count unit)
    (case (/ count unit)
      ((2/2 4/2)    'duple)
      ((3/2)        'triple-minor)
      ((3/1)        'triple-major) ; improve
      (else (throw "Bad meter count/unit" count unit)))))

(define select-mode
  (lambda (mood)
    (let* ([modes '((solemn . 1) (joyful . 10))] ; add others
           [mode (assq-ref modes mood)])
      (if (not mode) 
          (throw "Unrecognized mood" mood)
          (1- mode)))))

(define select-range-type
  (lambda (clefs)
    (screen-value clefs 'clefs '(default)))) ; add others

(define select-style
  (lambda (style)
    (screen-value style 'style '(simple)))) ; add others

(define correct-music
  (lambda (ls)
    (identity ls)))

(define-method
  (section->music (o <section>) (arca <arca>) style range)
  (let* ([meter-count   (slot-ref o 'meter-count)]
         [meter-unit    (slot-ref o 'meter-unit)]
         [mood          (slot-ref o 'mood)]
         [meter         (select-meter meter-count meter-unit)]
         [mode          (select-mode mood)]
         [sentences     (slot-ref o 'element)]
         [draft         (map (lambda (ls) 
                               (sentence->music ls arca style range meter mode)) 
                             sentences)])
    (correct-music draft)))

;(define write-mei
;  (lambda (text music)
;    (format #f "~a ~a" text music)))

(define-method
  (make-music (text <text>))

  (let* ([arca       +arca+]
         [style      (select-style       (slot-ref text 'style))]
         [range      (select-range-type  (slot-ref text 'clefs))]
         [sections   (slot-ref text 'element)]
         [music      (map (lambda (ls) 
                            (section->music ls arca style range)) 
                          sections)])
    music))
;    (write-mei text music)))
; + title, header info

