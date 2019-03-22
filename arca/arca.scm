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

;; TODO add rperms
;; add objects if needed

(define read-sxml
  (lambda (infile)
    (let* ([cmd (format #f "xmllint --xinclude ~a" infile)]
           [port (open-input-pipe cmd)]
           [xml (get-string-all port)]
           [sxml (xml->sxml xml)])
      sxml)))

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
          (list->vector ls))))  ; vector of integers
    ; or list?

    (define make-vperm
      (lambda (node)
        (let* ([voices ((sxpath '(// v *text*)) node)]
               [ls (map make-vnode voices)])
          (list->vector ls)))) ; vector of vectors

    (define make-vpermlist
      (lambda (col)
        (let* ([vpermlist (node-attr col 'permlist 'type "pitch")]
               [vperms ((sxpath '(// perm)) vpermlist)]
               [ls (map make-vperm vperms)])
          (list->vector ls)))) ; vector of vectors of vectors

    (define make-pinax
      (lambda (pinax-tree)
        (let* ([columns ((sxpath '(// column)) pinax-tree)]
               [ls (map make-vpermlist columns)])
          ls))) ; list of vectors of vectors of vectors

    (let* ([tree (read-sxml infile)]
           [pinakes ((sxpath '(// pinax)) tree)]
           [ls (map make-pinax pinakes)])
      (list->array '(0 0) ls)))) ; array of vectors (of vectors of vectors)

(define get-col
  (lambda (syntagma syl-count quantity)

    (define pinax-index
      (lambda (quantity)
        (let ([types '((long . 0) (short . 1))]) 
          (assq-ref types quantity))))

    (define col-index
      (lambda (syl-count) 
        (- syl-count 2)))

    (let* ([pinax (pinax-index quantity)]
           [col (col-index syl-count)])
      (array-ref syntagma pinax col))))

(define get-vperm
  (lambda (syntagma syl-count quantity)

    (define vperm-index
      (lambda (col)
        (let ([len (1- (vector-length col))]) 
          (random len (random-state-from-platform)))))

    (let* ([col (get-col syntagma syl-count quantity)]
           [vperm (vperm-index col)])
      (vector-ref col vperm))))

(define get-voice
  (lambda (vperm voice)
    (let* ([voices '((soprano . 0)
                     (alto . 1)
                     (tenor . 2)
                     (bass . 3))]
           [i (assq-ref voices voice)])
      (vector-ref vperm i))))
