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


(define-method
  (test-range (voice <voice>) (extreme <note>) (test <procedure>))
  (any (lambda (note) (test note extreme)) (element voice)))

(define-method
  (range-too-low? (voice <voice>) (extreme <note>))
  (test-range voice extreme note<?))

(define-method
  (range-too-high? (voice <voice>) (extreme <note>))
  (test-range voice extreme note>?))

(define-method
  (transpose! (voice <voice>) (interval <number>))
  (let* ([old (element voice)]
         [new (map (lambda (note) (inc note interval)) old)])
    (begin 
      (set! (element voice) new)
      voice)))

; TODO add chromatic equivalent
; add symbols for chromatic intervals (e.g. m6, a5)

(define octave-interval
  (lambda (n) (* 7 n)))

;(define range-extreme
;  (lambda (id type)
;    (let* ([range 
;             ; traditional clefs G2, C3, C4, F4
;             '((soprano . ((low . (g . 5)) (high . (d . 5))))
;               (alto    . ((low . (e . 4)) (high . (a . 4))))
;               (tenor   . ((low . (c . 3)) (high . (f . 4))))
;               (bass    . ((low . (f . 3)) (high . (b . 3)))))]
;           [voice (assq-ref range id)]
;           [pair (assq-ref voice type)]
;           [pname (car pair)]
;           [oct (cdr pair)])
;      (make <note> #:pname pname #:oct oct))))
;; put into voice object class?
;
;(define-method
;  (adjust-range-up! (voice <voice>))
;  (if (range-too-low? voice (range-extreme (id voice) 'low)) 
;      (begin 
;        (transpose! voice (octave-interval 1))
;        (adjust-range-up! voice)) 
;      voice))
;
;(define-method
;  (adjust-range-down! (voice <voice>))
;  (if (range-too-high? voice (range-extreme (id voice) 'high)) 
;      (begin 
;        (transpose! voice (octave-interval -1))
;        (adjust-range-down! voice))
;      voice))
;
;(define-method
;  (tritone? (voice1 <voice>) (voice2 <voice>))
;  "Boolean: Are there any tritones between the two voices?"
;  (let ([ls (zip (element voice1) (element voice2))])
;    (any (lambda (node) (tritone? (first node) (second node))) ls)))
;; better to report back the indexes of any tritones

