;; vim: set foldmethod=marker :

#|
lectio.scm
Andrew A. Cashner
2019-03-07--11
Read and process text input for Kircher's Arca musarithmica
|#

(use-modules
  (srfi srfi-1)
  (rnrs io ports)
  (ice-9 format)
  (ice-9 popen)
  (oop goops)
  (sxml simple)
  ((sxml xpath) #:renamer (symbol-prefix-proc 'sxp:)))

;; {{{1 DATA OBJECTS
;; {{{2 SYLLABLE
(define-class 
  <syl> (<string>)
  (str 
    #:init-value ""
    #:init-keyword #:str
    #:getter str)
  (quantity
    #:init-value 'short
    #:init-keyword #:quantity
    #:getter quantity)
  (wordpos
    #:init-value 'solo
    #:init-keyword #:wordpos
    #:accessor wordpos))

; could add:
;  (quality
;    #:init-value 'weak
;    #:init-keyword #:quality
;    #:getter quality)

(define-method 
  (smei (syl <syl>))
  (let ([str (str syl)]
        [pos (wordpos syl)])
    (if (eq? 'solo pos)
        `(mei:syl ,str) 
        `(mei:syl (@ (wordpos ,pos)) ,str))))

(define-method
  (mei (syl <syl>))
  (sxml->xml (smei syl)))

(define-method
  (write (syl <syl>) port)
  (display (mei syl) port))
;; }}}2

;; {{{2 WORD
(define-class
  <word> (<list>)
  (syl-ls
    #:init-value '()
    #:init-keyword #:syl-ls
    #:getter syl-ls))

(define-method
  (set-syl-positions! (word <word>))
  (let ([ls (syl-ls word)]
        [len (syl-count word)])
    (begin
      (for-each (lambda (syl) 
                  (set! (wordpos syl) 'm)) 
                (cdr ls))
      (set! (wordpos (first ls)) 'i)
      (set! (wordpos (last ls)) 't)))
  word)

(define-method
  (syl-count (word <word>))
  (length (syl-ls word)))

(define-method
  (long-position (word <word>))
  (let loop ([ls (reverse (syl-ls word))] [count 0])
    (if (or (null? ls) 
            (eq? (quantity (car ls)) 'long))
        count 
        (loop (cdr ls) (+ 1 count)))))

(define-method
  (penult-long? (word <word>))
  (eq? 1 (long-position word)))

(define-method
  (smei (word <word>))
  (map smei (syl-ls word)))

(define-method
  (mei (word <word>))
  (sxml->xml (smei word)))

(define-method
  (write (word <word>) port)
  (display (mei word) port))

;; }}}2

;; {{{2 PHRASE
(define-class 
  <phrase> (<list>)
  (word-ls 
    #:init-value '()
    #:init-keyword #:word-ls
    #:getter word-ls))

(define-method
  (syl-count (phrase <phrase>))
  (apply + (map syl-count (word-ls phrase))))

(define-method
  (word-count (phrase <phrase>))
  (length (syl-ls phrase)))

(define penult
  (lambda (ls)
    (first (cdr (reverse ls)))))

(define-method
  (penult-long? (phrase <phrase>))
  (let* ([ls (word-ls phrase)]
         [penult 
           (if (< (syl-count (last ls)) 2)
               ; last word = monosyllable, penult syl = last syl of penult word
               (last (syl-ls (penult ls)))
               ; last word = poly-syllabic, use penult syl of last word 
               (penult (syl-ls (car ls))))])
    (eq? (quantity penult) 'long)))


(define-method
  (smei (phrase <phrase>))
  (map smei (word-ls phrase)))

(define-method
  (mei (phrase <phrase>))
  (sxml->xml (smei phrase)))

(define-method
  (write (phrase <phrase>) port)
  (display (mei phrase) port))
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
    "Reduce consecutive whitespace chars to single space"
    (let loop ([ls (string->list str)] [new '()] [mode 'copy])
      (if (null? ls)
          (reverse-list->string new)

          (let ([this (first ls)])
            (cond [(eq? mode 'skip)
                   (if (whitespace? this)
                       (loop (cdr ls) new 'skip)
                       (loop (cdr ls) (cons this new) 'copy))]
                  [(eq? mode 'copy)
                   (if (whitespace? this)
                       (loop (cdr ls) (cons this new) 'skip) 
                       (loop (cdr ls) (cons this new) 'copy))]))))))

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
      (map word->obj ls))))

(define word->obj
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

          (let* ([syl-ls (map syl->obj ls)]
                 [word (make <word> #:syl-ls syl-ls)])
              (set-syl-positions! word))))


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
              (make <phrase> #:word-ls (reverse group))

              ; if the last element has a length less than shortest, reduce the
              ; longest size for the penultimate group so that the last element
              ; can be included in a group larger than shortest
              (let* ([longest (if (and (= (apply syl-count (cdr ls)) 1) 
                                       (< (syl-count (second ls)) shortest))
                                  (1- longest)
                                  longest)]
                     [next (first ls)]
                     [next-len (+ len (syl-count next))])
                (if (<= next-len longest)
                    (loop (cdr ls) next-len (cons next group))
                    (loop ls next-len group)))))))

      (let loop ([ls ls] [new '()])
        (if (null? ls)
            (reverse new)
            (let* ([head (make-phrase ls shortest longest)]
                   [tail (list-tail ls (word-count head))])
              (loop tail (cons head new)))))))
;; }}}1

;; {{{1 XML/SXML PROCESSING
(define read-xml-xinclude
  (lambda (infile)
    (let* ([text (call-with-input-file infile get-string-all)]
           ; Use xmllint to process xi:includes
           [xmllint-cmd (format #f "xmllint --xinclude ~a" infile)]
           [xmllint-port (open-input-pipe xmllint-cmd)]
           [xml (get-string-all xmllint-port)])
      (begin 
        (close-pipe xmllint-port) 
        xml))))

(define read-xml
  (lambda (infile)
    (let ([text (call-with-input-file infile get-string-all)])
      (xml->sxml text 
                 #:namespaces '((arca . "http://localhost")) 
                 #:trim-whitespace? #t))))

(define process-sxml
  (lambda (sxml)
    (let* ([style (car ((sxp:sxpath '(// arca:music @ style *text*)) sxml))]
           [clefs (car ((sxp:sxpath '(// arca:music @ clefs *text*)) sxml))]
           [sections ((sxp:sxpath '(// arca:music arca:section)) sxml)])

      (define get-lyrics
        (lambda (sxml)
          (car ((sxp:sxpath '(arca:lyrics *text*)) sxml))))

      (define str->phrases
        (lambda (str)
          (let* ([sentences (str->sentences str)]
                 [syllables (sentence->syllables str)]
                 [words (map word->obj syllables)])
            (map group-words words))))

      (let ([lyrics (map get-lyrics sections)])
        (map str->phrases lyrics)))))
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
                         - select syntagma based on arca:music/@style
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

