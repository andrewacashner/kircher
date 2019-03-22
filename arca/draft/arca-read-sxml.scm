;; procedures for reading data directly from XML
;; terribly inefficient!

(define read-sxml
  (lambda (infile)
    (let* ([cmd (format #f "xmllint --xinclude ~a" infile)]
           [port (open-input-pipe cmd)]
           [xml (get-string-all port)]
           [sxml (xml->sxml xml)])
      sxml)))


(define node-attr
  (lambda (tree node attr n)
    ((sxpath `(// (,node (@ (equal? (,attr ,n)))))) tree)))

(define read-column
  (lambda (syntagma syl-count quantity)
    (let* ([syl-num (number->string syl-count)] 
           [quant-nums '((long . "1") (short . "2"))]
           [pinax-num (assq-ref quant-nums quantity)]
           [pinax (node-attr syntagma 'pinax 'n pinax-num)])
      (node-attr pinax 'column 'syl syl-num))))

(define read-vperm
  (lambda (col)
    (let* ([i (random 9 (random-state-from-platform))] 
           [n (number->string i)]
           [permlist (node-attr col 'permlist 'type "pitch")])
    (node-attr permlist 'perm 'n n))))


