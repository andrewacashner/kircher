; cogito.scm
; Andrew A. Cashner
; 2019/03/13

(use-modules 
  (srfi srfi-1)
  (sxml simple)
  (oop goops))

(define rassoc
  (lambda (alist val pred?)
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
(define-method
  (8va (note <note>) . (n <number>))
  (let ([oct (if (null? n) 1 (car n))]) 
    (+ note (* 7 oct))))

(define-method
  (8vb (note <note>) . (n <number>))
  (let ([oct (if (null? n) 1 (car n))]) 
    (- note (* 7 oct))))

;original
(define-method
  (8va! (note <note>) . (n <number>))
  (let ([oct (if (null? n) 1 (car n))]) 
    (inc! note (* 7 oct))))

(define-method
  (8vb! (note <note>) . (n <number>))
  (let ([oct (if (null? n) 1 (car n))]) 
    (dec! note (* 7 oct))))


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
  (let ([pname (string (pname note))]
        [accid (ly-accid note)]
        [oct (ly-oct note)]
        [dur (number->string (dur note))]
        [dots (ly-dots note)]
        [ficta (ly-accid-ficta note)])
    (string-append pname accid oct dur dots ficta)))

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
  (let ([type "r"] ; TODO add full bar type
        [dur (number->string (dur rest))]
        [dots (ly-dots rest)])
    (string-append type dur dots)))
  
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
  (mei (voice <voice>))
  (sxml->xml (map smei (note-ls voice))))

(define-method
  (ly (voice <voice>)) 
  (string-join (map ly (note-ls voice)) " "))

(define-method
  (note-ref (voice <voice>) (i <number>))
  (list-ref (note-ls voice) i))

