;; vim: set foldmethod=marker :

;; lectio.scm
;; Andrew A. Cashner
;; 2019-03-07--11
;; Read and process text input for Kircher's Arca musarithmica

(use-modules
  (srfi srfi-1)
  (rnrs io ports)
  (ice-9 format)
  (oop goops)
  (sxml simple)
  ((sxml xpath) #:renamer (symbol-prefix-proc 'sxp:)))

;; {{{1 DATA OBJECTS
;; {{{2 ARCA and ARCALIST parent objects
(define set-if-valid-class!
  (lambda (obj slot class ls)

    (define class=?
      (lambda (class ls)
        (every (lambda (n) (is-a? n class)) ls)))

    (if (class=? class ls)
        (slot-set! obj slot ls)
        (throw 'invalid-class obj class ls))))


(define-class
  <arca:datum> ()
  (data
    #:init-value '()
    #:init-keyword #:data
    #:getter data))

(define-method
  (sxml (o <arca:datum>))
  (data o))

(define-method
  (write (o <arca:datum>) port)
  (sxml->xml (sxml o) port))

(define-class
  <arca:list> ()
  (data
    #:init-value '())
  (lst
    #:allocation #:virtual
    #:init-keyword #:lst
    #:accessor lst
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <arca:datum> ls))))

(define-method
  (sxml (o <arca:list>))
  (map sxml (lst o)))

(define-method
  (write (o <arca:list>) port)
  (sxml->xml (sxml o) port))

(define-method
  (element-count (o <arca:list>))
  (length (lst o)))
;; }}}2

;; {{{2 SYLLABLE
(define-class 
  <syl> (<arca:datum>)
  (data
    #:init-keyword #:str
    #:accessor str)
  (quantity
    #:init-value 'short
    #:init-keyword #:quantity
    #:getter quantity)
  (wordpos
    #:init-value 'solo
    #:init-keyword #:wordpos
    #:accessor wordpos))

(define-method
  (sxml (o <syl>))
  (if (eq? 'solo (wordpos o))
      `(syl (@ (label ,(quantity o))) ,(str o))
      `(syl (@ (label ,(quantity o)) 
               (wordpos ,(wordpos o))) ,(str o))))
;; }}}2

;; {{{2 WORD
(define-class 
  <word> (<arca:list>)
  (lst
    #:allocation #:virtual
    #:init-keyword #:lst
    #:accessor lst
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <syl> ls))))

(define-method
  (set-syl-positions! (word <word>))
  (let* ([ls (lst word)]
         [len (length ls)])
    (begin
      (for-each (lambda (syl) 
                  (set! (wordpos syl) 'm)) 
                (cdr ls))
      (set! (wordpos (first ls)) 'i)
      (set! (wordpos (last ls)) 't)))
  word)
;; }}}2

;; {{{2 PHRASE
(define-class 
  <phrase> (<arca:list>)
  (lst
    #:allocation #:virtual
    #:init-keyword #:lst
    #:accessor lst
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <word> ls))))

(define-method
  (syl-count (o <phrase>))
  (apply + (map element-count (lst o))))

(define penult
  (lambda (ls)
    (first (cdr (reverse ls)))))

(define-method
  (penult-long? (o <phrase>))
  (let* ([ls (lst o)]
         [penult 
           (if (< (length (last ls)) 2)
               ; last word = monosyllable, penult syl = last syl of penult word
               (last (lst (penult ls)))
               ; last word = poly-syllabic, use penult syl of last word 
               (penult (lst (car ls))))])
    (eq? (quantity penult) 'long)))
;; }}}2

;; {{{2 SENTENCE
(define-class
  <sentence> (<arca:list>)
  (lst
    #:allocation #:virtual
    #:init-keyword #:lst
    #:accessor lst
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <phrase> ls))))
;; }}}2

;; {{{2 TEXT-PROGRAM
(define-class
  <text-program> (<arca:list>)
  (lst
    #:allocation #:virtual
    #:init-keyword #:lst
    #:accessor lst
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <sentence> ls))))
;; }}}2
;; }}}1

;; {{{1 STRING PROCESSING
(define string-empty?
  (lambda (str)
    "Boolean: Does string have 0 length?"
    (= (string-length str) 0)))

(define whitespace? 
  (lambda (c) 
    (char-set-contains? char-set:whitespace c)))

(define end-punct?
  (lambda (c)
    "Boolean: Is character one of the specified close punctuation chars?"
    (let ([char-set:close-punct (string->char-set ".?!")])
      (char-set-contains? char-set:close-punct c))))

(define collapse-spaces
  (lambda (str)
    "Reduce consecutive whitespace chars to single space; remove non-space
    whitespace chars (e.g., #\newline)"

    (let loop ([ls (string->list str)] [new '()] [mode 'copy])
      (if (null? ls)
          (reverse-list->string new)

          (let ([this (first ls)])
            (cond [(eq? mode 'skip) 
                   (if (whitespace? this)
                       ; continue skipping 
                       (loop (cdr ls) new 'skip) 
                       ; start copying with this
                       (loop (cdr ls) (cons this new) 'copy))]

                  [(eq? mode 'copy)
                   (cond [(char=? this #\space) 
                          ; copy this and start skipping 
                          (loop (cdr ls) (cons this new) 'skip)]
                         [(whitespace? this)
                          ; continue-skipping 
                          (loop (cdr ls) new 'skip)]
                         [else 
                           ; continue copying 
                           (loop (cdr ls) (cons this new) 'copy)])]))))))

(define string-tokenize-keep-token
  (lambda (str tok)
    "Split string STR into a list of strings at occurences of token TOK 
    [can be any character predicate] like string-tokenize, but leave TOK at the
    end of each string"

    (let loop ([str str] [ls '()])
      (if (string-empty? str)
          (reverse ls)
          (let ([first-tok (string-index str tok)])
            (if (not first-tok)
                (loop "" (cons str ls))
                (let* ([split-index (+ 1 first-tok)]
                       [select (substring str 0 split-index)]
                       [tail (string-drop str split-index)])
                  (loop tail (cons select ls)))))))))
;; }}}1

;; {{{1 INPUT PROCESSING
(define ls->word
  (lambda (ls)
    "Given a list of syllables constituting a word,
    return a <word> object consisting of <syl> objects"

    (define syl->obj
      (lambda (str)
        "Create a <syl> object for STR"
        (let* ([first-char (string-ref str 0)]
               [quantity (if (char=? first-char #\`) 'long 'short)]
               [str (if (eq? quantity 'long) (string-drop str 1) str)])
          (make <syl> 
                #:str str 
                #:quantity quantity)))) ; set wordpos next 

          (let* ([ls (map syl->obj ls)]
                 [word (make <word> #:lst ls)])
              (begin
                (set-syl-positions! word)
                word))))

(define str->sentences
  (lambda (str)
    "Clean up string and split into a list of sentences."
    (let* ([trim (string-trim-both str char-set:whitespace)]
           [collapse (collapse-spaces trim)])
      (string-tokenize-keep-token collapse end-punct?))))

(define sentence->syllables
  (lambda (str)
    "Given sentence as string STR, return a list of words, where each word is a
    sub-list of its syllables"

    (define split-words
      (lambda (str)
        (string-split str char-set:whitespace)))

    (define split-syllables
      (lambda (str)
        (string-split str #\-)))

    (let* ([words (split-words str)]
           [ls (map split-syllables words)])
      (map ls->word ls))))

(define group-words
  (lambda (ls shortest longest)
    "Group a list of <word> objects into a list of <phrase> objects, where the
    sum of the syllable counts of the words in each phrase is s such that
    shortest <= s <= longest"

    (define make-phrase 
      (lambda (ls shortest longest)
        "Create a <phrase> object with list of the first elements of LS where
        the sum of their lengths is s such that shortest <= s <= longest"
        (let loop ([ls ls] [len 0] [group '()])
          (if (or (null? ls)
                  (>= len longest))
              (make <phrase> #:lst (reverse group))

              ; if the last element has a length less than shortest, reduce the
              ; longest size for the penultimate group so that the last element
              ; can be included in a group larger than shortest
              (let* ([longest (if (and (= (length (cdr ls)) 1) 
                                       (< (element-count (second ls)) shortest))
                                  (1- longest)
                                  longest)]
                     [next (first ls)]
                     [next-len (+ len (element-count next))])
                (if (<= next-len longest)
                    (loop (cdr ls) next-len (cons next group))
                    (loop ls next-len group)))))))

      (let loop ([ls ls] [new '()])
        (if (null? ls)
            (make <sentence> #:lst (reverse new))
            (let* ([head (make-phrase ls shortest longest)]
                   [tail (list-tail ls (element-count head))])
              (loop tail (cons head new)))))))
;; }}}1

;; {{{1 XML/SXML PROCESSING
(define read-xml
  (lambda (infile)
    (let ([text (call-with-input-file infile get-string-all)])
      (xml->sxml text 
                 #:namespaces '((arca . "http://localhost")) 
                 #:trim-whitespace? #t))))

; TODO use sxpath functions from arca
; verify output and connect to arca
(define make-text-program
  (lambda (sxml)

      (define str->phrases
        (lambda (str)
          (let* ([sentences (str->sentences str)]
                 [syllables (map sentence->syllables sentences)])
            (map (lambda (ls) (group-words ls 2 6)) syllables))))

    (let* ([style ((sxp:sxpath '(// arca:music @ style *text*)) sxml)]
           [clefs ((sxp:sxpath '(// arca:music @ clefs *text*)) sxml)]
           [lyrics ((sxp:sxpath '(// arca:lyrics *text*)) sxml)]
           [ls (map str->phrases lyrics)]
           [sections (map (lambda (ls) (make <text-program> #:lst ls)) ls)])
      sections)))
;; TODO integrate sections as well
;; make this a program that arca can interpret?
;; }}}1

;; {{{1 outline
#| 
for each section:
- store the meter and mood settings
- get the arca:lyrics element (assume 1 for now)
- parse the lyrics: break into sentences, then words, then syllables;
- group the sentences in groups of words according to syllable counts;
+ store data about accent/length in the syllables(long/short), words(position),
and phrases (penultimate value; later, poetic meter)

                         for each word group:
                         - select syntagma based on arca:arca:music/@style
                         - select pinax based on penult length
                         - select column based on syl count
                         - select vperm randomly 
                         - select rperm type based on meter
                         - select rperm randomly
                         - align notes and rhythms/rests; store in chorus/voice/note structures
                         - adjust notes for mode offset based on section/@mood and allowable modes for
                         this pinax

                         for the whole section now with notes:
                         - within voice: adjust intervals to avoid bad leaps
                         - adjust octaves and intervals based on voice ranges (music/@clefs) for each
                         voice and distance between voices
                         - add ficta accidentals to voices according to mode and context
                         - fix tritones, cross-relations between voices

                         - go to next section

                         - after all is done, output to arca:xml (modified MEI)
                         - use external xsl tools to convert arca:xml to mei
                         |#

                         ;; }}}1

