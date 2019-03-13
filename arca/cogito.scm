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
    #:init-value "cdefgab"
    #:getter pitchnames)
  (accid-names
    #:allocation #:class
    #:init-value '((natural . #\n)
                   (flat . #\f)
                   (sharp . #\s))
    #:getter accid-names)

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

  (accid-name ;set accid by char name instead of symbold, access char name
    #:allocation #:virtual
    #:init-keyword #:accid-name
    #:accessor accid-name
    #:slot-ref (lambda (note) 
                 (assq-ref (accid-names note) (accid note)))
    #:slot-set! (lambda (note c)
                  (set! (accid note) (rassoc (accid-names note) c char=?))))

  (std-pitch ; set pnum and oct from std-pitch, get std-pitch from pnum & octave
    #:allocation #:virtual
    #:init-keyword #:std-pitch
    #:accessor std-pitch
    #:slot-ref (lambda (note) 
                 (+ (* 7 (oct note)) (pnum note)))
    #:slot-set! (lambda (note std-pitch)
                  (begin
                    (set! (pnum note) (modulo std-pitch 7))
                    (set! (oct note) (floor (/ std-pitch 7)))))))

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
  (arithmetic (fn <procedure>) (note <note>) (n <number>))
  "Return a new note whose pnum and oct have been set by 
  applying procedure FN to NOTE with arg N"
  (let ([new (cp note)]
        [pitch (fn (std-pitch note) n)])
    (begin 
      (set! (std-pitch new) pitch)
      new)))

(define-method
  (+ (note <note>) (n <number>))
  "Increase pitch of NOTE by N"
  (arithmetic + note n))

(define-method
  (- (note <note>) (n <number>))
  "Decrease pitch of NOTE by N"
  (arithmetic - note n))

; YES mutating given note
(define-method
  (arithmetic! (fn <procedure>) (note <note>) (n <number>))
  "Return a new note whose pnum and oct have been set by 
  applying procedure FN to NOTE with arg N"
  (let ([pitch (fn (std-pitch note) n)])
      (set! (std-pitch note) pitch)
      note))

(define-method
  (inc! (note <note>) (n <number>))
  "Increase pitch of NOTE by N"
  (arithmetic! + note n))

(define-method
  (dec! (note <note>) (n <number>))
  "Decrease pitch of NOTE by N"
  (arithmetic! - note n))

; shift octaves
; copy
(define-method (8va (note <note>)) (+ note 7))
(define-method (8vb (note <note>)) (- note 7))

;original
(define-method (8va! (note <note>)) (inc! note 7))
(define-method (8vb! (note <note>)) (dec! note 7))


; difference of two notes
(define-method
  (- (note1 <note>) (note2 <note>))
  "Return integer difference between standard pitch of NOTE1 and NOTE2"
  (let ([p1 (std-pitch note1)]
        [p2 (std-pitch note2)])
   (- p1 p2)))

; compare
(define-method
  (cmp (fn <procedure>) (note1 <note>) (note2 <note>))
  (let ([p1 (std-pitch note1)]
        [p2 (std-pitch note2)])
    (fn p1 p2)))

(define-method
  (= (note1 <note>) (note2 <note>))
  (cmp = note1 note2))

(define-method
  (> (note1 <note>) (note2 <note>))
  (cmp > note1 note2))

(define-method
  (< (note1 <note>) (note2 <note>))
  (cmp < note1 note2))

(define-method
  (>= (note1 <note>) (note2 <note>))
  (cmp >= note1 note2))

(define-method
  (<= (note1 <note>) (note2 <note>))
  (cmp <= note1 note2))

; (TODO could also calculate chromatic intervals)


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
  (let ([pname (pname note)]
        [oct (oct note)]
        [dur (dur note)]
        [dots (dots note)]
        [accid (accid note)]
        [accid-name (accid-name note)]
        [accid-type (accid-type note)])
    (cond [(eq? accid-type 'default)
           (cond [(eq? accid 'natural)
                  `(note (@ (pname ,pname)
                            (oct ,oct)
                            (dur ,dur)
                            (dots ,dots)))]
                 [else 

                   `(note (@ (pname ,pname)
                             (oct ,oct)
                             (dur ,dur)
                             (accid ,accid-name)))])]
          [(eq? accid-type 'signature)
           `(note (@ (pname ,pname)
                     (oct ,oct)
                     (dur ,dur)
                     (accid.ges ,accid-name)))]
          [(eq? accid-type 'ficta)
           `(note (@ (pname ,pname)
                     (oct ,oct)
                     (dur ,dur))
                  (accid (@ (accid ,accid-name) 
                            (func "ficta"))))])))

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
; TODO could put these in class as virtual slots
; could allow note creation directly from MEI or Lilypond input!




            
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
    #:init-value '()
    #:init-keyword #:note-ls
    #:accessor note-ls))

(define-generic append)
(define-method
  (append (voice <voice>) (note <note>))
  (let* ([ls (reverse (note-ls voice))]
         [new (reverse (cons note ls))])
    (set! (note-ls voice) new)))

(define-method
  (smei (voice <voice>))
  `(voice (@ (id ,(id voice))
             (label ,(name voice)))
          (layer ,(map smei (note-ls voice)))))

(define-method
  (mei (voice <voice>))
  (sxml->xml (smei voice)))

(define-method
  (ly (voice <voice>)) 
  (format #f "\\new Voice = \"~a\" {\n~a\n}\n"
          (id voice) 
          (string-join (map ly (note-ls voice)) " ")))

(define-method
  (write (voice <voice>) port)
    (mei voice))
         


(define-method
  (note-ref (voice <voice>) (i <number>))
  (list-ref (note-ls voice) i))


(define-method
  (test-range (voice <voice>) (extreme <note>) (test <procedure>))
  (any (lambda (note) (test note extreme)) (note-ls voice)))

(define-method
  (range-too-low? (voice <voice>) (extreme <note>))
  (test-range voice extreme <))

(define-method
  (range-too-high? (voice <voice>) (extreme <note>))
  (test-range voice extreme >))

(define-method
  (transpose! (voice <voice>) (interval <number>))
  (let* ([old (note-ls voice)]
         [new (map (lambda (note) (+ note interval)) old)])
    (begin 
      (set! (note-ls voice) new)
      voice)))

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

