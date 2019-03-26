;; vim: set foldmethod=marker :

;; arca.scm
;; Andrew A. Cashner
;; 2019/03/15--25

(define-module 
  (kircher arca)
  #:use-module (kircher sxml)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:export (make-arca
             <arca>
             <syntagma>
             <pinax>
             <vpermlist>
             <vperm>
             <vnode>
             <rperm>
             get-syntagma
             get-col
             get-vperm
             get-rperm))

;; {{{1 OBJECTS and METHODS
;; {{{2 arca:vector
(define-class
  <arca:vector> ()
  (id
    #:init-value    'unset
    #:init-keyword  #:id
    #:getter        id)
  (desc 
    #:init-value    ""
    #:init-keyword  #:desc
    #:getter        desc)
  (data
    #:init-value    #())
  (element
    #:allocation    #:virtual
    #:init-keyword  #:element
    #:getter        element
    #:slot-ref      (lambda (o) (slot-ref o 'data))
    #:slot-set!     (lambda (o ls) (slot-set!  o 'data (list->vector ls)))))

(define-method
  (write (o <arca:vector>) port)
  (format port "~a" (element o)))

(define-method
  (arca-ref (o <arca:vector>) (i <integer>))
  (vector-ref (element o) i))

(define-method
  (arca-length (o <arca:vector>))
  (vector-length (element o)))
;; }}}2

;; {{{2 divisions of the arca
(define-class <arca> (<arca:vector>))

(define-class 
  <syntagma> (<arca:vector>)
  (style
    #:init-value    'unset
    #:init-keyword  #:style
    #:getter        style))

(define-class 
  <pinax> (<arca:vector>)
  (pred
    #:init-value    'identity
    #:init-keyword  #:pred
    #:getter        pred))

(define-class 
  <vpermlist> (<arca:vector>)
  (syl-count
    #:allocation    #:virtual
    #:slot-ref      (lambda (o) (arca-length (arca-ref (element o) 0)))
    #:slot-set!     (lambda (o n) (slot-set! o 'syl-count n))
    #:getter        syl-count))
; XXX or put syl-count at vperm level
; really need one layer above for column that includes rperm

(define-class <vperm> (<arca:vector>))

(define-class <vnode> (<arca:vector>))

(define-class <rperm> (<arca:vector>))

;; TODO add rperms
;; }}}2
;; }}}1

;; {{{1 READ AND STORE DATA FROM XML
(define make-syntagma
  (lambda (tree)
    
    (define make-vnode
      (lambda (str) ; single voice in vperm
        (let* ([str-ls  (string->list str)]
               [vals    (delq #\space str-ls)]
               [ls      (map (compose string->number string) vals)])
          (make <vnode> #:element ls)))) ; vector of integers

    (define make-vperm
      (lambda (node)
        (let* ([voices  (get-node node '*text*)]
               [ls      (map make-vnode voices)])
          (make <vperm> #:element ls)))) ; rank 2

    (define make-vpermlist
      (lambda (tree)
        (let* ([vperm-path  '(// (permlist (@ (equal? (type "pitch")))) perm)]
               [vpermlist   (path->node tree vperm-path)]
               [ls          (map make-vperm vpermlist)])
          (make <vpermlist> #:element ls)))) ; rank 3

    (define make-pinax
      (lambda (tree)
        (let* ([pred    (get-attr-text tree '// 'pred)]
               [columns (get-node tree 'column)]
               [ls      (map make-vpermlist columns)])
          (make <pinax> #:pred pred  #:element ls)))) ; rank 4

    (let* ([id      (get-attr-text tree '// 'n)]
           [desc    (get-attr-text tree '// 'desc)]
           [style   (get-attr-text tree '// 'style)]
           [pinakes (get-node tree 'pinax)]
           [ls      (map make-pinax pinakes)])
      (make <syntagma> 
            #:id id 
            #:desc desc
            #:style style
            #:element ls)))) ; rank 5

(define make-arca
  (lambda (infile)
    (let* ([tree (read-sxml-xinclude infile)]
           [syntagma (list (make-syntagma tree))]) ; for now, add others
      (make <arca> #:element syntagma))))

;; }}}1

;; {{{1 RETRIEVE DATA
(define-method
  (get-syntagma (arca <arca>) (style <symbol>))
  (let* ([syntagmata '((simple . 0))]
         [i (assq-ref syntagmata style)])
    (arca-ref arca i)))

(define-method 
  (get-col (syntagma <syntagma>) (syl-count <integer>) (quantity <symbol>))

    (define pinax-index
      (lambda (quantity)
        (let ([types '((long . 0) (short . 1))]) 
          (assq-ref types quantity))))
    (define col-index
      (lambda (syl-count) 
        (- syl-count 2)))

    (let* ([i-pinax (pinax-index quantity)]
           [i-col   (col-index syl-count)]
           [pinax   (arca-ref syntagma i-pinax)])
      (arca-ref pinax i-col)))

(define-method
  (get-vperm (syntagma <syntagma>) (syl-count <integer>) (quantity <symbol>))

    (define vperm-index
      (lambda (col)
        (let ([len (1- (vector-length (element col)))]) 
          (random len (random-state-from-platform)))))

    (let* ([col     (get-col syntagma syl-count quantity)]
           [vperm   (vperm-index col)])
      (arca-ref col vperm)))

(define-method 
  (get-voice (vperm <vperm>) (voice <symbol>))
    (let* ([voices '((soprano   . 0)
                     (alto      . 1)
                     (tenor     . 2)
                     (bass      . 3))]
           [i (assq-ref voices voice)])
      (arca-ref vperm i)))

(define-method
  (get-rperm (syntagma <syntagma>) (syl-count <integer>) (meter <symbol>))
  "DUMMY"
  #t)

;; }}}1

