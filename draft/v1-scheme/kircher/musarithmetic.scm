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
             adjust-initial-range
             adjust-range
             adjust-music
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

;;{{{1 utilities
(define screen-value
  (lambda (given type allowed)
    "Check if GIVEN is in list ALLOWED; if not throw exception; if yes return
    GIVEN"
    (if (not (member given allowed)) 
        (throw (format #f "Bad ~a value" type) given)
        given)))
;;}}}1

;;{{{1 mode
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
;;}}}1

;;{{{1 note arithmetic
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

(define octave-interval
  (lambda (n) (* 7 n)))

;; {{{2 compare
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
;;}}}2
;;}}}1

;;{{{1 adjust notes and voices
(define-method 
  (number-voices (o <chorus>))
  (let ([new (deep-clone o)])
    (let loop ([ls (element new)] [n 1])
      (if (null? ls)
          new 
          (begin
            (slot-set! (car ls) 'n n)
            (loop (cdr ls) (1+ n)))))))

;;{{{2 range
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
  (too-high? (o <note>) (range <symbol>) (voice-id <integer>))
  (let* ([extremes  (range-extremes range voice-id)]
         [high      (second extremes)])
    (note>? o high)))

(define-method
  (too-low? (o <note>) (range <symbol>) (voice-id <integer>))
  (let* ([extremes  (range-extremes range voice-id)]
         [low       (first extremes)])
    (note<? o low)))

(define-method
  (non-pitch? (o <note>))
  (or (is-a? o <rest>)
      (is-a? o <barLine>)))

(define-method 
  (adjust-initial-range (o <note>) (range <symbol>) (voice-id <integer>))
  "Adjust a note to be in the proper range for a given voice type; 
  Use highest possible range, Spanish style"
  (let* ([new       (deep-clone o)]
         [extremes  (range-extremes range voice-id)]
         [high      (second extremes)]
         [note      (inc-dia new (octave-interval (oct high)))])
    note))

(define-method
  (adjust-range (o <note>) (range <symbol>) (id <integer>)) 
  (if (non-pitch? o)
     o 
      (cond [(too-high? o range id) (8vb o)] 
            [(too-low?  o range id) (8va o)] 
            [else o])))
;;}}}2

;;{{{2 intervals
(define-method
  (adjust-music (o <voice>) (range <symbol>))
  (let* ([o2 (deep-clone o)]
         [id (1- (slot-ref o2 'n))]
         [ls (slot-ref o2 'element)])
    ; Iterate through elements of ls; compare next to prev (if both are
    ; pitches); adjust next if needed
    ; Use list 'new' like a stack to store results
    (let loop ([ls ls] [new '()] [prev '()])
      (if (null? ls)
          (begin 
            (slot-set! o2 'element (reverse new))
            o2)
          (let ([this (car ls)])
            (cond
              [(null? prev)
               ; First time around: If starting element is a <rest> or
               ; <barLine>, store it and move to next element, but don't compare
               ; to it (that is, pass prev as null); otherwise if it is a pitch,
               ; store it and pass it to compare with next
               (if (non-pitch? this)
                   (loop (cdr ls) (cons this new) '())
                   (loop (cdr ls) (cons this new) this))]
              ; this = <rest> or <barLine>? store this one but compare to prev
              [(non-pitch? this) 
               (loop (cdr ls) (cons this new) prev)]
              ; adjust intervals larger than a sixth by transposing new note
              [else 
                (let* ([diff (diff this prev)]
                       [this (cond [(> diff  5) (8vb this)]
                                   [(< diff -5) (8va this)]
                                   [else this])])
                  (loop (cdr ls) (cons this new) this))]))))))
#|                  (cond 
                    ; this out of range this means that prev was out of range,
                    ; so fix prev and try the same new note again
                    [(too-high? this range id) 
                     (loop ls new (8vb prev))]
                    [(too-low? this range id) 
                     (loop ls new (8va prev))]
                    ; if the new pitch is in a valid range, then store it and move
                    ; to next; use 'this' to compare
                    [else (loop (cdr ls) (cons this new) this)]))]))))))
|#
; TODO this method still does not allow you to go back more than one note to fix
; an interval or range problem.

(define-method
  (adjust-music (o <chorus>) (range <symbol>))
  (let* ([new    (deep-clone o)]
         [voices (element new)]
         [adj    (map (lambda (o) (adjust-music o range)) voices)])
    (begin
      (slot-set! new 'element adj)
      new)))
;;}}}2
;;}}}1
       
