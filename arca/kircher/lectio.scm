;; vim: set foldmethod=marker :

;; lectio.scm
;; Andrew A. Cashner
;; 2019-03-07--11
;; Read and process text input for Kircher's Arca musarithmica

(define-module
  (kircher lectio)
  #:use-module (kircher sxml)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (sxml simple)
  #:export (make-text 
             <text>
             <section>
             <sentence>
             <phrase>
             <word>
             <syl>
             get-section
             get-sentence
             get-phrase
             get-word
             get-syl
             syl-count
             penult-long?))

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
  <lectio:datum> ()
  (data
    #:init-value '()
    #:init-keyword #:data
    #:getter data))

(define-method
  (sxml (o <lectio:datum>))
  (data o))

(define-method
  (write (o <lectio:datum>) port)
  (sxml->xml (sxml o) port))

(define-class
  <lectio:list> ()
  (data
    #:init-value '())
  (element
    #:allocation #:virtual
    #:init-keyword #:element
    #:accessor element
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <lectio:datum> ls))))

(define-method
  (sxml (o <lectio:list>))
  (map sxml (element o)))

(define-method
  (write (o <lectio:list>) port)
  (sxml->xml (sxml o) port))

(define-method
  (element-count (o <lectio:list>))
  (length (element o)))
;; }}}2

;; {{{2 SYLLABLE
(define-class 
  <syl> (<lectio:datum>)
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
  `(syl (@ (label ,(quantity o)) 
           (wordpos ,(wordpos o))) 
        ,(data o)))
;; }}}2

;; {{{2 WORD
(define-class 
  <word> (<lectio:list>)
  (element
    #:allocation #:virtual
    #:init-keyword #:element
    #:accessor element
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <syl> ls))))

(define-method
  (set-syl-positions! (word <word>))
  (let* ([ls (element word)]
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
  <phrase> (<lectio:list>)
  (element
    #:allocation #:virtual
    #:init-keyword #:element
    #:accessor element
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <word> ls))))

(define-method
  (syl-count (o <phrase>))
  (apply + (map element-count (element o))))

(define penult
  (lambda (ls)
    (first (cdr (reverse ls)))))

(define-method
  (penult-long? (o <phrase>))
  (let* ([ls (element o)]
         [penult 
           (if (< (element-count (last ls)) 2)
               ; last word = monosyllable, penult syl = last syl of penult word
               (last (element (penult ls)))
               ; last word = poly-syllabic, use penult syl of last word 
               (penult (element (car ls))))])
    (eq? (quantity penult) 'long)))
;; }}}2

;; {{{2 SENTENCE
(define-class
  <sentence> (<lectio:list>)
  (element
    #:allocation #:virtual
    #:init-keyword #:element
    #:accessor element
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <phrase> ls))))
;; }}}2

;; {{{2 SECTION
(define-class
  <section> (<lectio:list>)
  (id 
    #:init-value ""
    #:init-keyword #:id
    #:getter id)
  (meter-count
    #:init-value 4
    #:init-keyword #:meter-count
    #:getter meter-count)
  (meter-unit
    #:init-value 2
    #:init-keyword #:meter-unit
    #:getter meter-unit)
  (mood
    #:init-value ""
    #:init-keyword #:mood
    #:getter mood)
  (element
    #:allocation #:virtual
    #:init-keyword #:element
    #:accessor element
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <sentence> ls))))
;; }}}2

;; {{{2 TEXT
(define-class
  <text> (<lectio:list>)
  (style
    #:init-value 'simple
    #:init-keyword #:style
    #:getter style)
  (clefs
    #:init-value 'default
    #:init-keyword #:clefs
    #:getter clefs)

  (element
    #:allocation #:virtual
    #:init-keyword #:element
    #:accessor element
    #:slot-ref (lambda (o) (slot-ref o 'data))
    #:slot-set! (lambda (o ls) 
                  (set-if-valid-class! o 'data <section> ls))))
;; }}}2

;; {{{2 RETRIEVE DATA
(define-method
  (lectio-ref (o <lectio:list>) (i <integer>))
  (list-ref (element o) i))

(define-method
  (get-section (o <text>) (i <integer>))
  (lectio-ref o i))

(define-method
  (get-sentence (o <section>) (i <integer>))
  (lectio-ref o i))

(define-method
  (get-phrase (o <sentence>) (i <integer>))
  (lectio-ref o i))

(define-method
  (get-word (o <phrase>) (i <integer>))
  (lectio-ref o i))

(define-method
  (get-syl (o <word>) (i <integer>))
  (lectio-ref o i))

(define-method
  (get-syl-str (o <syl>))
  (data o))
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
(define make-syl
  (lambda (str)
    "Create a <syl> object for STR"
    (let* ([first-char (string-ref str 0)]
           [quantity (if (char=? first-char #\`) 'long 'short)]
           [str (if (eq? quantity 'long) (string-drop str 1) str)])
      (make <syl> 
            #:data str 
            #:quantity quantity)))) ; set wordpos in make-word

(define make-word
  (lambda (ls)
    "Given a list of syllables constituting a word,
    return a <word> object consisting of <syl> objects"
    (let* ([word-ls (map make-syl ls)]
           [word (make <word> #:element word-ls)])
      (begin
        (set-syl-positions! word)
        word))))

(define make-phrase 
  (lambda (ls shortest longest)
    "Given a list of <word> objects, create a <phrase> object with list of the
    first elements of LS where the sum of their lengths is s such that shortest
    <= s <= longest"
    (let loop ([ls ls] [len 0] [group '()])
      (if (or (null? ls)
              (>= len longest))
          (make <phrase> #:element (reverse group))

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

(define make-sentence
  (lambda (str shortest longest)
    "Given a list of strings with hyphen-divided, accented words, create a
    <sentence> object which contains a list of <phrase> objects, in which each
    phrase is arranged so that the sum of the syllable counts of the words is s
    such that shortest <= s <= longest"
    (define sentence->words
      (lambda (str)
        "Given sentence as string STR, return a list of <word> objects,
        each containing a list of <syl> objects"
        (let* ([words (string-split str char-set:whitespace)]
               [ls (map (lambda (str) (string-split str #\-)) words)])
          (map make-word ls))))

      (let loop ([ls (sentence->words str)] [new '()])
        (if (null? ls)
            (make <sentence> #:element (reverse new))
            (let* ([head (make-phrase ls shortest longest)]
                   [tail (list-tail ls (element-count head))])
              (loop tail (cons head new)))))))

(define make-section
  (lambda (tree shortest longest)
    "Given an SXML tree of arca:section elements, create <section>
    objects for each, parsing all the subelements down to <word> and <syl>"
    (define str->sentences
      (lambda (str)
        "Clean up string and split into a list of sentences."
        (let* ([trim (string-trim-both str char-set:whitespace)]
               [collapse (collapse-spaces trim)])
          (string-tokenize-keep-token collapse end-punct?))))

    (let* ([id           (get-attr-text tree '// 'xml:id)]
           [meter-count  (get-attr-text tree '// 'meter.count)]
           [meter-unit   (get-attr-text tree '// 'meter.unit)]
           [mood         (get-attr-text tree '// 'mood)]
           [lyrics       (get-node-text tree 'arca:lyrics)]
           [sentences    (str->sentences lyrics)]
           [ls           (map (lambda (str) 
                                (make-sentence str shortest longest))
                              sentences)])
      (make <section>
            #:id          id
            #:meter-count (string->number meter-count)
            #:meter-unit  (string->number meter-unit)
            #:mood        (string->symbol mood)
            #:element     ls))))

(define make-text
  (lambda (infile)
    (let* ([tree (read-sxml infile)]
           [shortest 2]
           [longest  6]
           [style       (get-attr-text tree 'arca:music 'style)]
           [clefs       (get-attr-text tree 'arca:music 'clefs)]
           [sections    (get-node tree 'arca:section)]
           [ls          (map (lambda (ls) (make-section ls shortest longest))
                             sections)])
      (make <text> 
            #:style (string->symbol style)
            #:clefs (string->symbol clefs)
            #:element ls))))
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

