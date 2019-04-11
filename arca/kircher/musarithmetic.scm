;; vim: set foldmethod=marker :

;; musarithmetic.scm
;; Andrew A. Cashner
;; 2019/04/08

;; Calculate and adjust relationships of <note> objects
(define-module
  (kircher musarithmetic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs enums)
  #:use-module (oop goops)
  #:use-module (kircher sxml)
  #:use-module (kircher arca)
  #:use-module (kircher scribo)
  #:export (select-mode
             select-range-type
             select-style
             select-keysig
             adjust-mode
             adjust-range
             adjust-intervals
             number-voices))

#|
(use-modules
  (srfi srfi-1)
  (rnrs enums)
  (oop goops)
  (kircher sxml)
  (kircher arca)
  (kircher scribo))
|#

(define screen-value
  (lambda (given type allowed)
    "Check if GIVEN is in list ALLOWED; if not throw exception; if yes return
    GIVEN"
    (if (not (member given allowed)) 
        (throw (format #f "Bad ~a value" type) given)
        given)))


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
    (let* ([modes '((solemn . 0) (joyful . 11))] ; add others
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

(define select-keysig
  (lambda (mode)
    (if (mode-character? mode 'mollis) "1f" "0")))


(define-method
  (adjust-mode (o <note>) (mode <integer>))
  (let* ([adj (inc-dia o (mode-offset mode))]) 
    (cond [(and (= (slot-ref o 'pnum) 2) 
                ; Check for scale deg 3 before mode adjustment
                (mode-character? mode 'sharp3))
           (sharp adj 'default)]
          [(and (eq? (slot-ref adj 'pname) 'b) 
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

;(define-method
;  (set-octaves (o <chorus>))
;  (let ([new (deep-clone o)])
;    (let loop-voices ([voices (element new)] [n 5])
;      (if (null? voices)
;          new 
;          (let loop-notes ([notes (element (car voices))])
;            (if (null? notes)
;                (loop-voices (cdr voices) (1- n)) 
;                (begin
;                  (set! (oct (car notes)) n) 
;                  (loop-notes (cdr notes)))))))))

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

|#
;; }}}2

(define octave-interval
  (lambda (n) (* 7 n)))


(define +range-extreme-notes+
    ; traditional clefs G2, C3, C4, F4
    (let* ([extremes '(((d . 4) (g . 5))
                       ((e . 3) (a . 4))
                       ((c . 3) (f . 4))
                       ((f . 2) (b . 3)))]
           [notes   (map (lambda (ls) 
                           (map (lambda (pair) 
                                  (make <note> 
                                        #:pname (car pair)
                                        #:oct   (cdr pair))) ls)) extremes)])
      (list->vector notes)))

(define range-extremes
  (lambda (type voice-num)
    ; TODO assume default type for now 
    (vector-ref +range-extreme-notes+ voice-num)))

(define-method
  (adjust-range (o <note>) (range <symbol>) (voice-id <integer>))
  "Adjust a note to be in the proper range for a given voice type; 
  Use highest possible range, Spanish style"
  (let* ([new       (deep-clone o)]
         [extremes  (range-extremes range voice-id)]
         [low       (first extremes)]
         [high      (second extremes)]
         [start     (inc-dia new (octave-interval (oct low)))])
    (cond 
      [(note<? start low)  (8va start)]
      [(note>? start high) (8vb start)]
      [else start])))

(define-method 
  (adjust-range (o <voice>) range voice-id)
  (let* ([new (deep-clone o)]
         [ls  (element new)])
    (begin 
      (slot-set! new 'element 
               (map (lambda (n) (adjust-range n range voice-id)) ls))
      new)))

(define-method
  (adjust-range (o <chorus>) range)
  (let* ([new (deep-clone o)]
         [ls  (element new)])
    (begin
      (slot-set! new 'element
                 (map (lambda (v) 
                        (let ([voice-id (1- (length (member v ls)))])
                          (adjust-range v range voice-id)))
                      ls))
      new)))

(define-method
  (adjust-intervals (o <voice>))
  (let ([new (deep-clone o)])
    (let loop ([ls (slot-ref new 'element)] [adj '()])
      (if (null? ls)
          (begin
            (slot-set! new 'element (reverse adj))
            new)
          (if (null? (cdr ls))
              (loop (cdr ls) (cons (car ls) adj))
              (let* ([n1   (first ls)]
                     [n2   (second ls)]
                     [n1a  (if (> (diff n2 n1) 5) (8va n1) n1)]
                     [n2a  (if (> (diff n1 n2) 5) (8va n2) n2)])
                (loop (cddr ls) (append (list n2a n1a) adj))))))))

(define-method
  (adjust-intervals (o <chorus>))
  (let* ([new (deep-clone o)]
         [voices (element new)])
    (begin
      (slot-set! new 'element 
                 (map (lambda (o) (adjust-intervals o)) voices))
      new)))
                    

