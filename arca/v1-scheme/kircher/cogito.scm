;; vim: set foldmethod=marker :

;; cogito.scm
;; Andrew A. Cashner
;; 2019/03/13--04/05

;; improve mode adjustment, key signature, interval adjustment between notes and
;; voices

(define-module 
  (kircher cogito)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs enums)
  #:use-module (oop goops)
  #:use-module (sxml simple)
  #:use-module (kircher sxml)
  #:use-module (kircher arca)
  #:use-module (kircher lectio)
  #:use-module (kircher scribo)
  #:use-module (kircher musarithmetic)
  #:export (make-music))

#|
(use-modules 
  (srfi srfi-1)
  (oop goops)
  (sxml simple)
  (kircher sxml)
  (kircher lectio)
  (kircher arca))
|#

;; Read text input, calculate and compose music using arca
(define-method
  (make-rest (o <rnode>))
  (make <rest> #:dur (get-dur o)))

(define-method 
  (make-note (syl <syl>) pnum (rnode <rnode>) mode range voice-id)
  "Given syllable object from input file, pitch number from arca,
  and rhythmic permutation from arca, create <note> object;
  adjust note for mode and put in correct range for given voice"
  (let* ([pnum0     (1- pnum)] ; adjust to 0 index
         ; setting the absolute pitch allows us to deal with Kircher's pitch
         ; number 8 (0-index 7)
         [note      (make <note> 
                          #:pitch-dia   pnum0
                          #:dur         (get-dur rnode)
                          #:dots        (get-dots rnode)
                          #:syl         syl)]
         [note      (adjust-initial-range note range voice-id)]
         [note      (adjust-mode note mode)]
         [note      (adjust-range note range voice-id)])
    note))

(define-method
  (phrase->syl (o <phrase>))
  (let* ([words (slot-ref o 'element)]
         [syllables (map (lambda (o) (slot-ref o 'element)) words)])
    (flatten syllables)))

(define-method
  (music-combine (o <phrase>) voice rperm mode range voice-id)
  "Given a phrase of text, a list of voice nums, and a list of <rnode> objects,
  combine the three elements to make a list of <note> or <rest> objects that
  includes all the music and words, SATB, for that phrase"
  (let loop ([sls (phrase->syl o)] [vls voice] [rls rperm] [new '()])
    (if (null? sls)
        (let ([phrase (cons (make <barLine>) new)])
          (reverse phrase))
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
                               (let ([voice-id (1- (length (member v ls)))])
                               (music-combine o v rperm mode range voice-id)))
                             ls))])
         (reverse ls)))

(define-method
  (sentence->music (o <sentence>) (arca <arca>) style range meter mode)
  (let* ([ls (slot-ref o 'element)]
         [ls (fold-right 
               (lambda (this acc) 
                 (let ([this (phrase->music 
                               this arca style range meter mode)])
                   (cons this acc)))
               '()
               ls)]
         [ls (flatzip ls)])
    ls))

(define-method
  (section->music (o <section>) (arca <arca>) style range)
  (let* ([meter     (slot-ref o 'meter)]
         [mood      (slot-ref o 'mood)]
         [mode      (select-mode mood)]
         [keysig    (select-keysig mode)]
         [ls        (slot-ref o 'element)]
         [ls        (fold-right
                      (lambda (this acc)
                        (let ([this (sentence->music 
                                      this arca style range meter mode)])
                          (cons this acc)))
                      '()
                      ls)]
         [ls        (flatzip ls)]
         [ls        (map (lambda (ls) (make <voice> #:element ls)) ls)]
         [chorus    (make <chorus> #:element ls)]
         [chorus    (number-voices chorus)]
         [chorus    (adjust-music chorus range)])
    (make <music:section> 
          #:meter meter 
          #:keysig keysig
          #:element (list chorus))))

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

