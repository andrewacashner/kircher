;; vim: set foldmethod=marker :

;; arca.scm
;; Andrew A. Cashner
;; 2019/03/15

(use-modules
  (srfi srfi-1)
  (rnrs io ports)
  (ice-9 popen)
  (oop goops)
  (sxml simple))

;; {{{1 PERM

(define-class 
  <perm> ()
  (type
    #:init-value 'any
    #:init-keyword #:type
    #:getter type)
  (data
    #:init-form #()
    #:init-keyword #:data
    #:getter data))

(define-class 
  <vperm> ()
  (data
    #:init-value #2u8()
    #:init-keyword #:data
    #:getter data))

(define-class
  <column> ()
  (syl 
    #:init-value 0
    #:init-keyword #:syl
    #:getter syl)
  (vperms ; 3D array
    #:init-value #3u8()
    #:init-keyword #:vperms
    #:getter vperms)
  (rperms ; hashtable of vectors
    #:init-form (make-hash-table 3)
    #:init-keyword #:rperms
    #:getter rperms))

(let ([data "data/arca.xml"]
      [style 'simple]
      [syl-count 6]
      [penult 'long]
      [meter 'duple]
      [mode 1])
  (let* ([arca (make-arca data)]
         [syntagma (get-syntagma arca style)]
         [pinax (get-pinax syntagma penult)]
         [col (get-column pinax syl-count)]

         [vperm-index (random (length col))]
         [vperm (get-vperm col vperm-index)]

         [rpermlist (get-rpermlist col meter)]
         [rperm-index (random (length rpermlist))]
         [rperm (get-rperm rpermlist rperm-index)]

         [vperm-mode (get-pitches vperm mode)]
         [notes (make-notes vperm-mode rperm)])
    notes))

(define-method
  (get-syntagma (arca <arca>) (style <symbol>))
  (hashq-ref (table arca) style))

(define-method
  (get-pinax (syntagma <syntagma>) (penult <symbol>))
  (hashq-ref (table syntagma) penult))

(define-method
  (get-column (pinax <pinax>) (syl-count <integer>))
  (hashq-ref (table pinax) syl-count))

(define-method
  (get-vperm (column <column>) (index <integer>))
  (vector-ref (vperms column) index))

(define-method
  (get-rpermlist (column <column>) (meter <symbol>))
  (hashq-ref (rperms column) meter))

(define-method
  (get-rperm (rperms <vector>) (index <integer>))
  (vector-ref rperms index))

(define-method
  (get-pitches (vperm <vector>) (mode <integer>))
  (let ([voices (vector->list vperm)]
        [offset (mode-offset (1- mode))])
    (map (lambda (n) (inc-dial 7 n offset)) voices)))

(define mode-offset
  (lambda (mode)
    (let ([offsets #(1 2 3 4 5 6 7 8 9 10 11)])
      (vector-ref offsets mode))))

(define inc-dial
  (lambda (dial-max n m)
    "Increment like turning a dial, reset when going past dial-max"
    (modulo (+ n m) dial-max)))

(define-method
  (make-notes (vperm <list>) (rperm <list>))
  (let loop ([vperm vperm] [rperm rperm] [new '()])
    (if (or (null? rperm)
            (null? vperm))
        (reverse new)
        (let* ([pitch (car vperm)] 
               [this-r (car rperm)]
               [rhythm-type (car this-r)]
               [rhythm (cdr this-r)])
        (if (eq? rhythm-type 'rest)
            (loop vperm (cdr rperm) 
                  (cons (make-rest rhythm) new))
            (loop (cdr vperm) (cdr rperm) 
                  (cons (make-note pitch rhythm) new)))))))

(define get-dur
  (lambda (val)
    "Return numeric value of symbol, ignoring dot ('sbd = 'sb)"
    (let* ([str (symbol->string val)]
           [trim (string-take str 2)]
           [sym (string->symbol str)]
           [table (alist->hashtable
                    '((br . "breve")
                      (sb . "1")
                      (mn . "2")
                      (sm . "4")
                      (fs . "8")))]
           (hashq-ref table val)))))

(define get-dots
  (lambda (val)
    "Return 1 dot if symbol ends with #\d, else 0"
    (let ([str (string-reverse (symbol->string val))])
      (if (char=? (string-ref str 0) #\d) 1 0))))
 
(define make-rest
  (lambda (val)
    (make <rest> 
          #:dur (get-dur val) 
          #:dots (get-dots val))))

(define make-note
  (lambda (pnum val)
    (make <note> 
          #:pnum pnum 
          #:dur (get-dur val)
          #:dots (get-dots val))))

#|
<arca>: hashtable containing <syntagma> objects tagged by style
<syntagma>: hashtable containing <pinax> objects tagged by penult-type
<pinax>: hashtable containing <column> objects tagged by syl-count
 (could all be subclasses of same class)

<column>: object containing <vpermlist> object and <rpermlist> object

<vpermlist>: vector of <vperm> objects

<rpermlist>: hashtable of <rperm> objects tagged by meter
  (could be subclass of same class as pinax)

<vperm>: 2d int array of voice numbers, 4 x syl-count (or list?)

<rperm>: vector of <rhythm> objects (or just symbols, see below)
<rhythm>: object with slots for type (rest or played) and value (symbol)
  (or just a symbol, and look for #\r like you did for dots)
|#

(define-class
  <pinax> ()
  (n
    #:init-value 0
    #:init-keyword #:n
    #:getter n)
;  (pred
;    #:init-form (lambda (x) (identity x))
;    #:init-keyword #:pred
;    #:getter pred)
  (desc
    #:init-value ""
    #:init-keyword #:desc
    #:getter desc)
  (data
    #:init-form (make-hash-table 10)
    #:init-keyword #:data
    #:getter data))
#|
(define-class 
  <syntagma> ()
  (n
    #:init-value 0
    #:init-keyword #:n
    #:getter n)
  (style
    #:init-value 'simple
    #:init-keyword #:style
    #:getter style)
  (desc
    #:init-value ""
    #:init-keyword #:desc
    #:getter desc)
  (pinaxes
    #:allocation #:virtual
    #:init-keyword #:pinaxes
    #:accessor pinaxes
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls)
                  (set-if-valid-class! o 'data <pinax> ls))))
|#

