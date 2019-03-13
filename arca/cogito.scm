; cogito.scm
; Andrew A. Cashner
; 2019/03/13

(use-modules 
  (oop goops))

(define-class 
  <note> ()
  (pnum 
    #:init-value 'unset
    #:init-keyword #:pnum
    #:accessor pnum)
  (oct 
    #:init-value 'unset
    #:init-keyword #:oct
    #:accessor oct)
  (accid 
    #:init-value 'unset
    #:init-keyword #:accid
    #:accessor accid)
  (accid-type 
    #:init-value 'unset
    #:init-keyword #:accid-type
    #:accessor accid-type)
  (dur 
    #:init-value 'unset
    #:init-keyword #:dur
    #:accessor dur)
  (std-pitch
    #:allocation #:virtual
    #:getter std-pitch
    #:slot-ref (lambda (note) 
                 (+ (* 7 (oct note)) (pnum note)))
    #:slot-set! (lambda (note oct pnum)
                  (slot-set! note 'std-pitch
                             (+ (* 7 (oct note)) (pnum note))))))




(define-class
  <rest> ()
  (dur 
    #:init-value 'unset
    #:init-keyword #:dur
    #:accessor dur))

(define-class
  <voice> (<list>)
  (id 
    #:init-value 'unset
    #:init-keyword #:id
    #:accessor id)
  (note-ls 
    #:init-value '()
    #:init-keyword #:note-ls
    #:accessor note-ls))



