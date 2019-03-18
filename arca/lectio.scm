; lectio.scm
; Andrew A. Cashner
; 2019-03-07--11
; Read and process text input for Kircher's Arca musarithmica

(use-modules
  (srfi srfi-1)
  (rnrs io ports)
  (ice-9 format)
  (ice-9 popen)
  (oop goops)
  (sxml simple)
  ((sxml xpath) #:renamer (symbol-prefix-proc 'sxp:)))

;; DATA OBJECTS

; SYLLABLE
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
  (quality
    #:init-value 'weak
    #:init-keyword #:quality
    #:getter quality)
  (wordpos
    #:init-value 'solo
    #:init-keyword #:wordpos
    #:accessor wordpos))


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


; WORD
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


; PHRASE
(define-class 
  <phrase> (<list>)
  (word-ls 
    #:init-value '()
    #:init-keyword #:word-ls
    #:getter word-ls))

(define-method
  (syl-count (phrase <phrase>))
  (apply + (map syl-count (word-ls phrase))))

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

;; STRING PROCESSING
(define string-empty?
  (lambda (str)
    "Boolean: Does string have 0 length?"
    (= (string-length str) 0)))

(define end-punct?
  (lambda (c)
    "Boolean: Is character one of the specified close punctuation chars?"
    (define End-punct-chars ".?!")
    (let ([char-set:close-punct (string->char-set End-punct-chars)])
      (char-set-contains? char-set:close-punct c))))

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

;; INPUT and PROCESSING
(define clean-text-ls
  (lambda (ls)
    "Given list of strings, remove blank and comment strings."

    (define Comment-char #\%)

    (define not-comment?
      (lambda (str) 
        (let ([first-char (string-ref str 0)])
          (not (char=? first-char Comment-char)))))

    (define not-blank?
      (lambda (str) 
        (not (string-empty? str))))

    (define strip-blank-lines 
      (lambda (ls) 
        (filter not-blank? ls)))

    (define strip-comments 
      (lambda (ls) 
        (filter not-comment? ls)))

    (let ([no-blanks (strip-blank-lines ls)])
      (strip-comments no-blanks))))

(define strip-whitespace-str
  (lambda (str) 
    (string-trim-both str char-set:whitespace)))

(define collapse-spaces
  (lambda (str)
    "Reduce consecutive spaces to a single space"
    (let loop ([ls (string->list str)] [new '()] [mode 'copy])
      (if (null? ls)
          (reverse-list->string new)
          (let ([this (first ls)])
            (cond [(eq? mode 'skip)
                   (if (char=? this #\space)
                       (loop (cdr ls) new 'skip)
                       (loop (cdr ls) (cons this new) 'copy))]
                  [(eq? mode 'copy)
                   (if (char=? this #\space)
                       (loop (cdr ls) (cons this new) 'skip) 
                       (loop (cdr ls) (cons this new) 'copy))]))))))

(define clean-spaces
  (lambda (str)
    (collapse-spaces (strip-whitespace-str str))))

(define str->sentences
  (lambda (str)
    "Given string, split into list of newline-separated strings,
    remove blank and comment lines, merge string again and separate at
    sentences, remove leading and trailing whitespace."
    
    (let* ([lines (string-split str #\newline)]
           [clean-lines (clean-text-ls lines)]
           [new-text (string-join clean-lines)]
           [sentences (string-tokenize-keep-token new-text end-punct?)])
      (map strip-whitespace-str sentences))))


(define sentence->syllables
  (lambda (str)
    "Given sentence as string STR, return a list of words, where each word is a
    sub-list of its syllables"

    (define split-words
      (lambda (str)
        (string-split str char-set:whitespace)))

    (define split-syllables
      (lambda (str)
        (define Syllable-delimiter #\-)
        (string-split str Syllable-delimiter)))

    (map split-syllables (split-words str))))


(define group-words
  (lambda (ls shortest longest)
    "Given a list-of-lists LS, split into sublists, where the sum of the 
    lengths of the sub-sublists in each sublist is s such that shortest <= s <= longest;
    given a list of words divided into syllables, return a list of
    word groups where each group has no fewer syllables than MIN and no more syllables than MAX"

    (define make-group
      (lambda (ls shortest longest)
        "Create a list of the first elements of LS where the sum of their lengths 
        is s such that shortest <= s <= longest"
        (let loop ([old ls] [len 0] [group '()])
          (if (or (null? old)
                  (>= len longest))
              (reverse group)
              ; if the last element has a length less than shortest, reduce the
              ; longest size for the penultimate group so that the last element
              ; can be included in a group larger than shortest
              (let* ([longest (if (and (= (length (cdr old)) 1) 
                                   (< (length (cadr old)) shortest))
                              (- longest 1)
                              longest)]
                     [next (car old)]
                     [next-len (+ len (length next))])
                (if (<= next-len longest)
                    (loop (cdr old) next-len (cons next group))
                    (loop old next-len group)))))))

    (let loop ([old ls] [new '()])
      (if (null? old)
          (reverse new)
          (let* ([head (make-group old shortest longest)]
                 [tail (list-tail old (length head))])
            (loop tail (cons head new)))))))

(define word-groups->arca
  (lambda (ls shortest longest)
    "Given a list of word groups, return a list of structures with syllable
    count, penultimate syllable quantity, and text for each group"
(define group-word-ls (lambda (ls)
        (group-words ls shortest longest)))

    (define make-syl
      (lambda (str) ; single string
        (let ([first-char (string-ref str 0)])
          (if (char=? first-char #\') 
              (make <syl> #:str (string-drop str 1) #:quantity 'long)
              (make <syl> #:str str)))))

    (define make-word
      (lambda (ls) ; list of syllable strings
        (let ([syl-ls (map make-syl ls)])
          (set-syl-positions! (make <word> #:syl-ls syl-ls)))))

    (define make-phrase
      (lambda (ls) ; list (phrase) of list (word) of list (syl)
        (let ([word-ls (map make-word ls)])
          (make <phrase> #:word-ls word-ls))))

    (define make-sentence
      (lambda (ls)
        (map make-phrase ls)))

    (define make-text
      (lambda (ls)
        (map make-sentence ls)))

    (let ([groups (map group-word-ls ls)]) 
      (make-text groups))))

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

      (let* ([lyrics (map get-lyrics sections)]
             [lyrics-clean (map clean-spaces lyrics)]
             [ls (map str->sentences lyrics-clean)]
             [syl-ls (map (lambda (subls) (map sentence->syllables subls)) ls)]
             [group-ls (map (lambda (subls) 
                              (map 
                                (lambda (ssubls) (group-words ssubls 2 6))
                                   subls)) syl-ls)]) ; TODO you can do better!
        group-ls)))) ; TODO convert to arca structures

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



