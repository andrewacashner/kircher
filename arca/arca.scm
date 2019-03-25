;; vim: set foldmethod=marker :

;; arca.scm
;; Andrew A. Cashner
;; 2019/03/15--25

(use-modules
  (srfi srfi-1)
  (rnrs io ports)
  (ice-9 popen)
  (oop goops)
  (sxml simple)
  (sxml xpath))

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

; TODO (define-class <rperm> (<arca:vector>))

;; TODO add rperms
;; }}}2
;; }}}1

;; {{{1 SXPATH FUNCTIONS
(define path->node
  (lambda (tree path)
    ((sxpath path) tree)))

(define get-node
  (lambda (tree node)
    (path->node tree `(// ,node))))

(define node->text
  (lambda (node)
    (path->node node '(// *text*))))

(define get-node-text
  (lambda (tree node)
    (let ([node (get-node tree node)])
      (node->text node))))

(define get-attr-text
  (lambda (tree node attr)
    (let ([node (path->node tree `(// ,node @ ,attr))])
      (node->text node))))
;; }}}1

;; {{{1 INPUT XML and STORE DATA
(define read-sxml
  (lambda (infile)
    (let* ([cmd     (format #f "xmllint --xinclude ~a" infile)]
           [port    (open-input-pipe cmd)]
           [xml     (get-string-all port)]) 
      (xml->sxml xml 
                 #:namespaces '((arca . "http://localhost"))
                 #:trim-whitespace? #t))))

(define make-syntagma
  (lambda (tree)
    
    (define make-vnode
      (lambda (str) ; single voice in vperm
        (let* ([ls      (string->list str)]
               [vals    (delq #\space ls)]
               [ls      (map (compose string->number string) vals)])
          (make <vnode> #:element ls)))) ; vector of integers

    (define make-vperm
      (lambda (node)
        (let* ([voices  (node->text node)]
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
        (let* ([pred    (get-attr-text tree 'pinax 'pred)]
               [columns (get-node tree 'column)]
               [ls      (map make-vpermlist columns)])
          (make <pinax> #:pred pred  #:element ls)))) ; rank 4

    (let* ([id      (get-attr-text tree 'syntagma 'n)]
           [desc    (get-attr-text tree 'syntagma 'desc)]
           [style   (get-attr-text tree 'syntagma 'style)]
           [pinakes (get-node tree 'pinax)]
           [ls      (map make-pinax pinakes)])
      (make <syntagma> 
            #:id id 
            #:desc desc
            #:style style
            #:element ls)))) ; rank 5
;; }}}1

;; {{{1 RETRIEVE DATA
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
;; }}}1

