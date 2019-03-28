;; vim: set foldmethod=marker :

;; arca.scm
;; Andrew A. Cashner
;; 2019/03/15--25

(use-modules
  (kircher sxml)
  (srfi srfi-1)
  (oop goops))

;(define-module 
;  (kircher arca)
;  #:use-module (kircher sxml)
;  #:use-module (srfi srfi-1)
;  #:use-module (oop goops)
;  #:export (make-arca
;             <arca>
;             <syntagma>
;             <pinax>
;             <column>
;             <vpermlist>
;             <vperm>
;             <vnode>
;             <rpermlist>
;             <rperm>
;             get-syntagma
;             get-column
;             get-vperm
;             get-rperm))
;
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
  <column> (<arca:vector>)
  (vpermlist ; alias for element (and thus for data)
    #:allocation    #:virtual
    #:init-keyword  #:vpermlist
    #:slot-ref      (lambda (o) (slot-ref o 'data))
    #:slot-set!     (lambda (o v) (slot-set! o 'data v))
    #:getter        vpermlist)
  (data2
    #:init-value #())
  (rpermlist ; alias for element2
    #:allocation    #:virtual
    #:init-keyword  #:rpermlist
    #:slot-ref      (lambda (o) (slot-ref o 'data2))
    #:slot-set!     (lambda (o v) (slot-set! o 'data2 v))
    #:getter        rpermlist)
  (syl-count 
    #:allocation    #:virtual
    #:slot-ref      (lambda (o) (syl-count (vpermlist o)))
    #:slot-set!     (lambda (o n) (slot-set! o 'syl-count n))
    #:getter        syl-count))

(define-method
  (write (o <column>) port)
  (format port "~a ~a" (vpermlist o) (rpermlist o)))
(define-class 
  <vpermlist> (<arca:vector>)
  (syl-count
    #:allocation    #:virtual
    #:slot-ref      (lambda (o) (arca-length (arca-ref (element o) 0)))
    #:slot-set!     (lambda (o n) (slot-set! o 'syl-count n))
    #:getter        syl-count))

(define-class <vperm> (<arca:vector>))

(define-class <vnode> (<arca:vector>))

(define-class 
  <rpermlist> (<arca:vector>)
  (meter 
    #:init-value 'duple
    #:init-keyword #:meter
    #:getter meter))

(define-class <rperm> (<arca:vector>))
(define-class <rnode> (<arca:vector>))
;; TODO fix
;; }}}2
;; }}}1

;; {{{1 READ AND STORE DATA FROM XML
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

(define make-rnode
  (lambda (str)
    (let* ([str-ls  (string-split str char-set:whitespace)]
           [ls      (map string->symbol str-ls)])
      (make <rnode> #:element ls))))

(define make-rperm
  (lambda (node)
    (let* ([vals    (get-node node '*text*)]
           [ls      (map make-rnode vals)])
      (make <rperm> #:element ls))))
; TODO nested too deep?

(define make-rpermlist-meter
  (lambda (tree type)
    "Make a single <rpermlist> object with <rperm> objects for a single meter"
    (let* ([type-str    (symbol->string type)] 
           [path        `(// (permlist (@ (equal? (type ,type-str)))) perm)]
           [rperms      (path->node tree path)]
           [ls          (map make-rperm rperms)])
      (make <rpermlist> #:meter type  #:element ls))))

(define make-rpermlist-all
  (lambda (tree)
    "Make a single <rpermlist> object containing <rpermlist> objects for the
    three meter categories"
    (let* ([rpermlist-tree   
             (path->node tree '(// (permlist (@ (equal? (type "rhythm"))))))]
           [duple           (make-rpermlist-meter rpermlist-tree 'duple)]
           [triple-major    (make-rpermlist-meter rpermlist-tree 'triple-major)]
           [triple-minor    (make-rpermlist-meter rpermlist-tree 'triple-minor)]
           [ls              (list duple triple-major triple-minor)])
      (make <rpermlist> #:element ls))))

(define make-column
  (lambda (tree)
    (let ([vpermlist (make-vpermlist tree)] 
          [rpermlist (make-rpermlist-all tree)])
      (make <column> 
            #:vpermlist vpermlist 
            #:rpermlist rpermlist))))

(define make-pinax
  (lambda (tree)
    (let* ([pred    (get-attr-text tree '// 'pred)]
           [columns (get-node tree 'column)]
           [ls      (map make-column columns)])
      (make <pinax> #:pred pred  #:element ls)))) ; rank 4

(define make-syntagma
  (lambda (tree)
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
  (get-pinax (syntagma <syntagma>) (quantity <symbol>))
      (let* ([types '((long . 0) (short . 1))]
             [index (assq-ref types quantity)])
        (arca-ref syntagma index)))

(define-method 
  (get-column (pinax <pinax>) (syl-count <integer>))
  (let ([index   (- syl-count 2)])
    (arca-ref pinax index)))

(define-method
  (get-vpermlist (o <column>))
  (vpermlist o))

(define-method
  (get-vperm (column <column>))
  (let* ([vpermlist (get-vpermlist column)]
         [len       (1- (arca-length vpermlist))]
         [index     (random len (random-state-from-platform))])
    (arca-ref vpermlist index)))

(define-method
  (get-rpermlist (o <column>) (meter <symbol>))
  (let* ([all-rpermlist (rpermlist o)] 
         [indices '((duple          . 0)
                    (triple-major   . 1)
                    (triple-minor   . 2))]
         [index (assq-ref indices meter)])
    (arca-ref all-rpermlist index)))

(define-method
  (get-rperm (column <column>) (meter <symbol>))
  (let* ([rpermlist (get-rpermlist column meter)]
         [len       (arca-length rpermlist)]
         [index     (random len (random-state-from-platform))])
    (arca-ref rpermlist index)))

(define-method
  (get-rhythm (rperm <rperm>))
  (arca-ref rperm 0))
;; fix XXX

(define-method 
  (get-voice (vperm <vperm>) (voice <symbol>))
  (let* ([voices '((soprano   . 0)
                   (alto      . 1)
                   (tenor     . 2)
                   (bass      . 3))]
         [i (assq-ref voices voice)])
    (arca-ref vperm i)))
;; }}}1

