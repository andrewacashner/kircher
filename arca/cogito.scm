; cogito.scm
; Andrew A. Cashner
; 2019/03/13

(use-modules 
  (srfi srfi-1)
  (sxml simple)
  (oop goops))

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

; ******************************************************
; CLASSES and METHODS

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

  ; Global lookup keys for class
  (pitchnames
    #:allocation #:class
    #:getter pitchnames
    #:init-value "cdefgab")
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

; note arithmetic
; not mutating given note
(define-method 
  (cp (note <note>))
  (make <note> 
        #:pnum (pnum note) 
        #:oct (oct note) 
        #:dur (dur note)
        #:dots (dots note)
        #:accid (accid note)
        #:accid-type (accid-type note)))

(define-method
  (arithmetic (fn <procedure>) (access <accessor>) (note <note>) (n <number>))
  "Return a new note whose pnum and oct have been set by 
  applying procedure FN to NOTE with arg N"
  (let ([new (cp note)]
        [pitch (fn (access note) n)])
    (begin 
      (set! (access new) pitch)
      new)))

(define-method
  (inc (note <note>) (n <number>))
  "Increase pitch of NOTE by N"
  (arithmetic + pitch-dia note n))

(define-method
  (dec (note <note>) (n <number>))
  "Decrease pitch of NOTE by N"
  (arithmetic - pitch-dia note n))

(define-method
  (inc-chrom (note <note>) (n <number>))
  (arithmetic + pitch-chrom note n))

(define-method
  (dec-chrom (note <note>) (n <number>))
  (arithmetic - pitch-chrom note n))


; YES mutating given note
(define-method
  (arithmetic! (fn <procedure>) (access <accessor>) (note <note>) (n <number>))
  "Return a new note whose pnum and oct have been set by 
  applying procedure FN to NOTE with arg N"
  (let ([pitch (fn (access note) n)])
      (set! (access note) pitch)
      note))

(define-method
  (inc! (note <note>) (n <number>))
  "Increase pitch of NOTE by N"
  (arithmetic! + pitch-dia note n))

(define-method
  (dec! (note <note>) (n <number>))
  "Decrease pitch of NOTE by N"
  (arithmetic! - pitch-dia note n))

(define-method
  (inc-chrom! (note <note>) (n <number>))
  "Increase pitch of NOTE by N"
  (arithmetic! + pitch-chrom note n))

(define-method
  (dec-chrom! (note <note>) (n <number>))
  "Decrease pitch of NOTE by N"
  (arithmetic! - pitch-chrom note n))


; shift octaves
; copy
(define-method (8va (note <note>)) (inc note 7))
(define-method (8vb (note <note>)) (dec note 7))

;original
(define-method (8va! (note <note>)) (inc! note 7))
(define-method (8vb! (note <note>)) (dec! note 7))


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
  (write (note <note>) port)
  (format port "~c~d:~d~s~c[~a]"
          (pname note)
          (oct note)
          (dur note)
          (ly-dots note)
          (accid-name note)
          (accid-type note)))

(define-method
  (smei (note <note>))
  (let* ([pname  `(pname ,(pname note))]
         [oct    `(oct ,(oct note))]
         [dur    `(dur ,(dur note))]
         [dots   `(dots ,(dots note))]
         [accid-name (accid note)] 
         [accid-char (mei-accid note)]
         [accid-type (accid-type note)])

    (cond [(eq? accid-type 'default) 
           (if (eq? accid-name 'natural)
               ; no accidental
               `(note (@ ,pname ,oct ,dur ,dots))
               ; accidental as attribute
               `(note (@ ,pname ,oct ,dur ,dots 
                         (accid ,accid-char))))]

          [(eq? accid-type 'signature) 
           ;accid.ges as attribute
           `(note (@ ,pname ,oct ,dur ,dots 
                     (accid.ges ,accid-char)))]

          [(eq? accid-type 'ficta) 
           ;accidental as child element with func attribute
           `(note (@ ,pname ,oct ,dur ,dots) 
                  (accid (@ (accid ,accid-char) (func "ficta"))))])))

(define-method
  (mei (note <note>)) 
  (sxml->xml (smei note)))

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
  (mei-accid (note <note>))
  (let ([alist '((natural . #\n)
                 (flat . #\f)
                 (sharp . #\s))]) 
    (assq-ref alist (accid note))))

; TODO could put these in class as virtual slots
; could allow note creation directly from MEI or Lilypond input!

; ****************************************************************
; REST
(define-class <rest> (<note>))

(define-method
  (smei (rest <rest>))
  `(rest (@ (dur ,(dur rest))
            (dots ,(dots rest)))))

(define-method
  (ly (rest <rest>))
  (format #f "~c~d~a"
          #\r ; add full-bar type
          (dur rest)
          (ly-dots rest)))
  
(define-method
  (write (rest <rest>) port)
  (format port "r~d" (dur rest)))

; ****************************************************************
; VOICE

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

(define-generic append)
(define-method
  (append (voice <voice>) (note <note>))
  (let* ([ls (reverse (notes voice))]
         [new (reverse (cons note ls))])
    (set! (notes voice) new)))

(define-method
  (smei (voice <voice>))
  `(voice (@ (id ,(id voice))
             (label ,(name voice)))
          (layer ,(map smei (notes voice)))))

(define-method
  (mei (voice <voice>))
  (sxml->xml (smei voice)))

(define-method
  (ly (voice <voice>)) 
  (format #f "\\new Voice = \"~a\" {\n~a\n}\n"
          (id voice) 
          (string-join (map ly (notes voice)) " ")))

(define-method
  (write (voice <voice>) port)
    (mei voice))
         


(define-method
  (note-ref (voice <voice>) (i <number>))
  (list-ref (notes voice) i))


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
