;; vim: set foldmethod=marker :

;; musarithmetic.scm
;; Andrew A. Cashner
;; 2019/04/08

;; Calculate and adjust relationships of <note> objects

(define mode-offset
  (lambda (n)
    (let ([offsets  ; diatonic
            ; d g a a Bb f g g d a c f 
            #(1 4 5 5 6 3 4 4 1 5 0 3)])
      (vector-ref offsets n))))

(define mode-character?
  (lambda (n query)
    (let* ([tests '((sharp3 . (3))
                    (flat6  . (0 1 8)) 
                    (sharp7 . (0 1 2 6 7 8))
                    (mollis . (1 4 5 8 11)))]
           [test  (assq-ref tests query)])
      (if (member n test) #t #f))))


(define select-mode
  (lambda (mood)
    (let* ([modes '((solemn . 0) (joyful . 9))] ; add others
           [mode (assq-ref modes mood)])
      (if (not mode) 
          (throw "Unrecognized mood" mood)
          mode))))

(define select-range-type
  (lambda (clefs)
    (screen-value clefs 'clefs '(default)))) ; add others

(define select-style
  (lambda (style)
    (screen-value style 'style '(simple)))) ; add others


(define-method
  (adjust-mode (o <note>) (mode <integer>))
  (let* ([adj (inc-dia o (mode-offset mode))]) 
    (cond [(and (= (slot-ref o 'pnum) 2) 
                ; Check for scale deg 3 before mode adjustment
                (mode-character? mode 'sharp3))
           (sharp adj 'default)]
          [(and (eq? (slot-ref adj 'pname) #\b) 
                ; Check for note=B after mode adjustment
                (mode-character? mode 'mollis))
           (flat adj 'signature)]
          [else adj])))
; ficta flat6 and sharp7 depend on context, need to be done at <chorus> level
; or higher

;; note arithmetic
;; not mutating given note
(define-method
  (arithmetic (fn <procedure>) (access <accessor>) (note <note>) (n <number>))
  "Return a new <note> with slot of accessor set to result of applying FN
  with argument N"
  (let* ([new    (deep-clone note)]
         [result (fn (access new) n)])
    (begin 
      (set! (access new) result)
      new)))

(define-method
  (inc-dia (o <note>) (n <number>))
  "Increase pitch of <note> by N diatonic steps"
  (arithmetic + pitch-dia o n))

(define-method
  (inc-chrom (o <note>) (n <number>))
  "Increase pitch of <note> by N chromatic steps"
  (arithmetic + pitch-chrom o n))



(define-method
  (accid-alter (o <note>) (accid <symbol>) (type <symbol>))
  "Set accid and type of <note> O"
  (let ([new (deep-clone o)])
    (begin 
      (slot-set! new 'accid accid)
      (slot-set! new 'accid-type type))
    new))
    
(define-method
  (flat (o <note>) type)
  (accid-alter o 'flat type))

(define-method
  (sharp (o <note>) type)
  (accid-alter o 'sharp type))

(define-method 
  (number-voices (o <chorus>))
  (let ([new (deep-clone o)])
    (let loop ([ls (element new)] [n 1])
      (if (null? ls)
          new 
          (begin
            (slot-set! (car ls) 'n n)
            (loop (cdr ls) (1+ n)))))))

(define-method
  (set-octaves (o <chorus>))
  (let ([new (deep-clone o)])
    (let loop-voices ([voices (element new)] [n 5])
      (if (null? voices)
          new 
          (let loop-notes ([notes (element (car voices))])
            (if (null? notes)
                (loop-voices (cdr voices) (1- n)) 
                (begin
                  (set! (oct (car notes)) n) 
                  (loop-notes (cdr notes)))))))))

; shift octaves
(define-method (8va (note <note>)) (inc-dia note 7))
(define-method (8vb (note <note>)) (inc-dia note -7))

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

;(define-method
;  (tritone? (voice1 <voice>) (voice2 <voice>))
;  "Boolean: Are there any tritones between the two voices?"
;  (let ([ls (zip (element voice1) (element voice2))])
;    (any (lambda (node) (tritone? (first node) (second node))) ls)))
;; better to report back the indexes of any tritones

;; {{{2 test and adjust entire <voice>
#|
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
  (transpose (o <voice>) (interval <number>))
  (let* ([new   (deep-clone o)]
         [ls    (element o)]
         [adj   (map (lambda (n) (inc note interval)) ls)])
    (begin 
      (set! (element new) adj)
      new)))

; TODO add chromatic equivalent
; add symbols for chromatic intervals (e.g. m6, a5)

(define octave-interval
  (lambda (n) (* 7 n)))
|#
;; }}}2
(define range-extreme
  (lambda (type voice-num dir)
    (let* ([range
             ; traditional clefs G2, C3, C4, F4
             #(#(#((d . 4) (g . 5)) 
                 #((e . 3) (a . 4))
                 #((c . 3) (f . 4))
                 #((f . 2) (b . 3))))]
           [type    0]                           ;(list-index '(default other) type)] ; TODO doesn't work
           [dir     (if (eq? dir 'low) 0 1)]     ;(list-index '(low high other) dir)]
           [set     (vector-ref range type)]
           [voice   (vector-ref set voice-num)]
           [pair    (vector-ref voice dir)]
           [pname   (car pair)]
           [oct     (cdr pair)])
      (make <note> #:pname pname #:oct oct))))

(define-method
  (adjust-range (o <note>) (range <symbol>) (voice-id <integer>))
  (let* ([new   (deep-clone o)]
         [low   (range-extreme range voice-id 'low)]
         [high  (range-extreme range voice-id 'high)]
         [start (set! (oct new) (oct low))]) 
    (if (note<? start low) (8va start) start)))



