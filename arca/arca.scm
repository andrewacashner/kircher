; arca.scm
; Andrew A. Cashner
; 2019/03/15

(use-modules
  (srfi srfi-1)
  (rnrs io ports)
  (ice-9 popen)
  (oop goops)
  (sxml simple))

(define set-if-valid-class!
  (lambda (obj slot class ls)
; XXX use the one in lectio.scm
    (define class=?
      (lambda (class ls)
        (every (lambda (n) (is-a? n class)) ls)))

    (if (class=? class ls)
        (slot-set! obj slot ls)
        (throw 'invalid-class obj class ls))))

(define-class 
  <syntagma> (<list>)
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
  (pinax-ls
    #:init-value '())
  (pinaxes
    #:allocation #:virtual
    #:init-keyword #:pinaxes
    #:accessor pinaxes
    #:slot-ref (lambda (o) (slot-ref o 'pinax-ls))
    #:slot-set! (lambda (o ls)
                  (set-if-valid-class! o 'pinax-ls <pinax> ls))))

(define-class 
  <pinax> (<list>)
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
  (column-ls
    #:init-value '())
  (columns
    #:allocation #:virtual
    #:init-keyword #:columns
    #:accessor columns
    #:slot-ref (lambda (o) (slot-ref o 'column-ls))
    #:slot-set! (lambda (o ls)
                  (set-if-valid-class! o 'column-ls <column> ls))))

(define-class 
  <column> (<list>)
  (n 
    #:init-value 0
    #:init-keyword #:n
    #:getter n)
  (syl 
    #:init-value 0 
    #:init-keyword #:syl
    #:getter syl)
  (vperm-ls
    #:init-value '())
  (vperms
    #:allocation #:virtual
    #:init-keyword #:vperms
    #:accessor vperms
    #:slot-ref (lambda (o) (slot-ref o 'vperm-ls))
    #:slot-set! (lambda (o ls)
                  (set-if-valid-class! o 'vperm-ls <vperm> ls)))
  (rperm-ls
    #:init-value '())
  (rperms
    #:allocation #:virtual
    #:init-keyword #:rperms
    #:accessor rperms
    #:slot-ref (lambda (o) (slot-ref o 'rperm-ls))
    #:slot-set! (lambda (o ls)
                  (set-if-valid-class! o 'rperm-ls <rperm> ls))))

(define-class 
  <permlist> (<list>)
  (type
    #:init-value 'pitch
    #:init-keyword #:type
    #:getter type)
  (perm-ls
    #:init-value '())
  (perms
    #:allocation #:virtual
    #:init-keyword #:perms
    #:accessor perms
    #:slot-ref (lambda (o) (slot-ref o 'perm-ls))
    #:slot-set! (lambda (o ls)
                  (set-if-valid-class! o 'perm-ls <vperm> ls))))
; will this work for vperms and rperms?

(define-class 
  <perm> (<list>)
  (n
    #:init-value 0
    #:init-keyword #:n
    #:getter n)
  (data
    #:init-value '()
    #:init-keyword #:data
    #:accessor data))

(define-class 
  <vperm> (<perm>)
  (voices
    #:allocation #:virtual
    #:init-keyword #:voices
    #:accessor voices
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <vperm-voice> ls))))

(define-class
  <vperm-voice> (<list>)
  (n
    #:init-value 0
    #:init-keyword #:n
    #:getter n)
  (voicenum-ls
    #:init-value '())
  (voicenums
    #:allocation #:virtual
    #:init-keyword #:voicenums
    #:accessor voicenums
    #:slot-ref (lambda (o) (slot-ref o 'voicenum-ls))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'voicenum-ls <number> ls))))

(define-class 
  <rperm> (<perm>)
  (meter
    #:init-value 'duple
    #:init-keyword #:meter
    #:getter meter)
  (rhythm-ls
    #:init-value '())
  (rhythms
    #:allocation #:virtual
    #:init-keyword #:rhythms
    #:accessor rhythms
    #:slot-ref (lambda (o) (slot-ref o 'rhythm-ls))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'rhythm-ls <symbol> ls))))

; define mode lookup as a virtual slot on vperm-voice?
; define rhythm value lookup as virtual slot on rperm?


