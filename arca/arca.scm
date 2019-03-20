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

;; {{{1 duplicate with lectio classes
; XXX use the one in lectio.scm
(define set-if-valid-class!
  (lambda (obj slot class ls)
    (define class=?
      (lambda (class ls)
        (every (lambda (n) (is-a? n class)) ls)))

    (if (class=? class ls)
        (slot-set! obj slot ls)
        (throw 'invalid-class obj class ls))))

(define-class
  <arca> ()
  (data
    #:init-value '()
    #:init-keyword #:data
    #:getter data))

(define-method
  (sxml (o <arca>))
  (data o))

(define-method
  (write (o <arca>) port)
  (sxml->xml (sxml o) port))


(define-class
  <arcalist> ()
  (data
    #:init-value '())
  (lst
    #:allocation #:virtual
    #:init-keyword #:lst
    #:accessor lst
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <arca> ls))))

(define-method
  (sxml (o <arcalist>))
  (map sxml (lst o)))

(define-method
  (write (o <arcalist>) port)
  (sxml->xml (sxml o) port))

(define-method
  (element-count (o <arcalist>))
  (length (lst o)))
;; }}}1

;; {{{1 PERM

(define-class <perm> (<arca>))

(define-class 
  <permlist> (<arcalist>)
  (type
    #:init-value 'pitch
    #:init-keyword #:type
    #:getter type)
  (lst
    #:allocation #:virtual
    #:init-keyword #:lst
    #:accessor lst
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls)
                  (set-if-valid-class! o 'data <perm> ls))))

(define-class 
  <vperm> ()
  (data
    #:init-value #2u8()
    #:init-keyword #:data
    #:getter data))

(define-class
  <rperm> ()
  (type
    #:init-value 'duple
    #:init-keyword #:type
    #:getter type))

(define-class
  <column> ()
  (syl)
  (vperms)
  (rperms)
  )

(define-class 
  <pinax> ()
  (n
    #:init-value 0
    #:init-keyword #:n
    #:getter n)
  (pred
    #:init-form (lambda (x) (identity x))
    #:init-keyword #:pred
    #:getter pred)
  (desc
    #:init-value ""
    #:init-keyword #:desc
    #:getter desc)
  (columns
    #:allocation #:virtual
    #:init-keyword #:columns
    #:accessor columns
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls)
                  (set-if-valid-class! o 'data <column> ls))))

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


