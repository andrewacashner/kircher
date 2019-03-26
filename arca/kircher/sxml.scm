;; vim: set foldmethod=marker :

;; module (kircher sxml)
;; Andrew A. Cashner
;; 2019/03/26

(define-module
  (kircher sxml)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 popen)
  #:use-module (sxml simple)
  #:use-module (sxml xpath)
  #:export  (read-sxml 
             read-sxml-xinclude
             path->node
             get-node
             get-node-text
             get-attr-text))

(setlocale LC_ALL "")

;; {{{1 read, convert input
(define arca:xml->sxml
  (lambda (xml)
    (xml->sxml xml
               #:namespaces '((arca . "http://localhost")) 
               #:trim-whitespace? #t)))

(define read-sxml
  (lambda (infile)
    (let ([xml (call-with-input-file infile get-string-all)])
      (arca:xml->sxml xml))))

(define read-sxml-xinclude
  (lambda (infile)
    (let* ([cmd     (format #f "xmllint --xinclude ~a" infile)]
           [port    (open-input-pipe cmd)]
           [xml     (get-string-all port)]) 
      (arca:xml->sxml xml))))
;; }}}1

;; {{{1 sxpath functions
(define path->node
  (lambda (tree path)
    ((sxpath path) tree)))

(define node->text
  (lambda (node)
    (car (path->node node '(// *text*)))))

(define get-node
  (lambda (tree node)
    (path->node tree `(// ,node))))

(define get-node-text
  (lambda (tree node)
    (let ([node (get-node tree node)])
      (node->text node))))

(define get-attr-text
  (lambda (tree node attr)
    (let ([node (path->node tree `(// ,node @ ,attr))])
      (node->text node))))
;; }}}1

