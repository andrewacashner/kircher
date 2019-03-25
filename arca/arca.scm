;; vim: set foldmethod=marker :

;; arca.scm
;; Andrew A. Cashner
;; 2019/03/15

(use-modules
  (srfi srfi-1)
  (rnrs io ports)
  (ice-9 popen)
  (oop goops)
  (sxml simple)
  (sxml xpath))

(define-class
  <arca:vector> ()
  (id
    #:init-value 'unset
    #:init-keyword #:id
    #:getter id)
  (desc 
    #:init-value ""
    #:init-keyword #:desc
    #:getter desc)
  (data
    #:init-value #())
  (element
    #:allocation #:virtual
    #:init-keyword #:element
    #:getter element
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls)
                    (slot-set! o 'data (list->vector ls)))))

(define-method
  (arca-ref (o <arca:vector>) (i <integer>))
    (vector-ref (element o) i))

(define-class <syntagma> (<arca:vector>))

(define-class <pinax> (<arca:vector>))

(define-class <vpermlist> (<arca:vector>))

(define-class <vperm> (<arca:vector>))

(define-class <vnode> (<arca:vector>))

; TODO (define-class <rperm> (<arca:vector>))

;; TODO add rperms

(define read-sxml
  (lambda (infile)
    (let* ([cmd (format #f "xmllint --xinclude ~a" infile)]
           [port (open-input-pipe cmd)]
           [xml (get-string-all port)]
           [sxml (xml->sxml xml)])
      sxml)))

; TODO also set id, desc slots from sxml
(define make-syntagma
  (lambda (infile)

    (define node-attr
      (lambda (tree node attr n)
        ((sxpath `(// (,node (@ (equal? (,attr ,n)))))) tree)))

    (define make-vnode
      (lambda (str) ; single voice in vperm
        (let* ([ls (string->list str)]
               [vals (delq #\space ls)]
               [ls (map (compose string->number string) vals)])
          (make <vnode> #:element ls)))) ; vector of integers

    (define make-vperm
      (lambda (node)
        (let* ([voices ((sxpath '(// v *text*)) node)]
               [ls (map make-vnode voices)])
          (make <vperm> #:element ls)))) ; rank 2

    (define make-vpermlist
      (lambda (col)
        (let* ([vpermlist (node-attr col 'permlist 'type "pitch")]
               [vperms ((sxpath '(// perm)) vpermlist)]
               [ls (map make-vperm vperms)])
          (make <vpermlist> #:element ls)))) ; rank 3

    (define make-pinax
      (lambda (pinax-tree)
        (let* ([columns ((sxpath '(// column)) pinax-tree)]
               [ls (map make-vpermlist columns)])
          (make <pinax> #:element ls)))) ; rank 4

    (let* ([tree (read-sxml infile)]
           [pinakes ((sxpath '(// pinax)) tree)]
           [ls (map make-pinax pinakes)])
      (make <syntagma> #:element ls)))) ; rank 5

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
           [i-col (col-index syl-count)]
           [pinax (arca-ref syntagma i-pinax)])
      (arca-ref pinax i-col)))

(define-method
  (get-vperm (syntagma <syntagma>) (syl-count <integer>) (quantity <symbol>))

    (define vperm-index
      (lambda (col)
        (let ([len (1- (vector-length (element col)))]) 
          (random len (random-state-from-platform)))))

    (let* ([col (get-col syntagma syl-count quantity)]
           [vperm (vperm-index col)])
      (arca-ref col vperm)))

(define-method 
  (get-voice (vperm <vperm>) (voice <integer>))
    (let* ([voices '((soprano . 0)
                     (alto . 1)
                     (tenor . 2)
                     (bass . 3))]
           [i (assq-ref voices voice)])
      (arca-ref vperm i)))


