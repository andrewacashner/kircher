;; vim: set foldmethod=marker :

;; arca.scm
;; Andrew A. Cashner
;; 2019/03/15--28

(define-module 
  (kircher arca)
  #:use-module (kircher sxml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs enums)
  #:use-module (oop goops)
  #:export (make-arca
             <arca>
             <syntagma>
             <pinax>
             <column>
             <vpermlist>
             <vperm>
             <vnode>
             <rpermlist>
             <rperm>
             <rnode>
             get-syntagma
             get-pinax
             get-column
             get-vperm
             get-voice
             get-rperm
             get-rnode
             dur
             dots))

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

(define-class 
  <rpermlist> (<arca:vector>)
  (meter 
    #:init-value    'duple
    #:init-keyword  #:meter
    #:getter        meter))

(define-class <vnode> (<arca:vector>))

(define duration-symbols
  '(br sb mn sm fs brd sbd mnd smd fsd 
       rbr rsb rmn rsm rfs rbrd rsbd rmnd rsmd rfsd))
(define duration-vec 
  (list->vector duration-symbols))

(define duration-nums (make-enumeration duration-symbols))
(define e-duration (enum-set-indexer duration-nums))

(define rest-nums
  ((enum-set-constructor duration-nums) 
   '(rbr rsb rmn rsm rfs rbrd rsbd rmnd rsmd rfsd)))
(define dot-nums
  ((enum-set-constructor duration-nums)
   '(brd sbd mnd smd fsd rbrd rsbd rmnd rsmd rfsd)))
(define breve-nums
  ((enum-set-constructor duration-nums)
   '(br brd rbr rbrd)))
(define semibreve-nums
  ((enum-set-constructor duration-nums)
   '(sb sbd rsb rsbd)))
(define minim-nums
  ((enum-set-constructor duration-nums)
   '(mn mnd rmn rmnd)))
(define semiminim-nums
  ((enum-set-constructor duration-nums)
   '(sm smd rsm rsmd)))
(define fusa-nums
  ((enum-set-constructor duration-nums)
   '(fs fsd rfs rfsd)))


(define dur-sym (lambda (n) (vector-ref duration-vec n)))
(define dur-num (lambda (s) (e-duration s)))

(define-class <rperm> (<arca:vector>))

(define-class
  <rnode> ()
  (num
    #:init-value    0 
    #:init-keyword  #:num
    #:getter        num)
  (sym
    #:allocation    #:virtual
    #:init-keyword  #:sym
    #:getter        sym
    #:slot-ref      (lambda (o) (dur-sym (num o)))
    #:slot-set!     (lambda (o sym) 
                      (let ([num (dur-num sym)])
                        (if (not num) 
                            (throw 'invalid-rnode-symbol sym) 
                            (slot-set! o 'num num))))))

(define-method
  (write (o <rnode>) port)
  (format port "~d" (num o)))


(define-method
  (dur (o <rnode>))
  (let ([n (sym o)])
    (cond 
      [(enum-set-member? n breve-nums)      "breve"]
      [(enum-set-member? n semibreve-nums)  "1"]
      [(enum-set-member? n minim-nums)      "2"]
      [(enum-set-member? n semiminim-nums)  "4"]
      [(enum-set-member? n fusa-nums)       "8"]
      [else (throw 'no-output-string-for-dur-code n)])))

(define-method
  (dots (o <rnode>))
  (if (enum-set-member? (dur-sym (num o)) dots) 1 0))
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
  (lambda (s)
    (make <rnode> #:sym s)))
   
(define make-rperm
  (lambda (node)
    (let* ([vals    (get-node-text node '//)]
           [str     (string-split vals char-set:whitespace)]
           [sym     (map string->symbol str)]
           [ls      (map make-rnode sym)])
      (make <rperm> #:element ls))))

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
    (let* ([tree        (read-sxml-xinclude infile)]
           [syntagmata  (get-node tree 'syntagma)]
           [ls          (map make-syntagma syntagmata)])
      (make <arca> #:element ls))))
;; }}}1

;; {{{1 RETRIEVE DATA
(define-method
  (get-syntagma (arca <arca>) (style <symbol>))
  (let* ([syntagmata    (make-enumeration '(simple))]
         [index         ((enum-set-indexer syntagmata) style)])
    (arca-ref arca index)))

(define-method
  (get-pinax (syntagma <syntagma>) (quantity <symbol>))
      (let* ([types (make-enumeration '(long short))]
             [index ((enum-set-indexer types) quantity)])
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
         [meters    (make-enumeration '(duple triple-major triple-minor))]
         [index     ((enum-set-indexer meters) meter)])
    (arca-ref all-rpermlist index)))

(define-method
  (get-rperm (column <column>) (meter <symbol>))
  (let* ([rpermlist (get-rpermlist column meter)]
         [len       (arca-length rpermlist)]
         [index     (random len (random-state-from-platform))])
    (arca-ref rpermlist index)))

(define-method
  (get-rnode (o <rperm>) (i <integer>))
    (dur (arca-ref o i)))

(define-method 
  (get-voice (vperm <vperm>) (voice <symbol>))
  (let* ([voices (make-enumeration '(soprano alto tenor bass))]
         [index  ((enum-set-indexer voices) voice)])
    (arca-ref vperm index)))
;; }}}1

