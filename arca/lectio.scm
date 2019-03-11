; lectio.scm
; Andrew A. Cashner
; 2019-03-07--11
; Read and process text input for Kircher's Arca musarithmica

; ALSO:
; Read text file with syllables divided with hyphens and (optionally)
; accented/quantities marked with #\', convert to Lilypond and MEI lyrics format

(use-modules
  (ice-9 format)
  (rnrs io ports)
  (srfi srfi-1)
  (oop goops)
  (sxml simple))

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
  (ly (syl <syl>))
  (str syl))

(define-method
  (write (syl <syl>) port)
  (format port "<syl: ~a>" (str syl)))

(define-method 
  (sxml-mei (syl <syl>))
  (let ([str (str syl)]
        [pos (wordpos syl)])
    (if (eq? 'solo pos)
        `(mei:syl ,str) 
        `(mei:syl (@ (wordpos ,pos)) ,str))))

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
  (ly (word <word>))
  (let ([str-ls (map str (syl-ls word))])
    (string-join str-ls " -- ")))

(define-method
  (sxml-mei (word <word>))
  (map sxml-mei (syl-ls word)))

(define-method
  (mei (word <word>))
  (sxml->xml (sxml-mei word)))


(define-method
  (write (word <word>) port)
  (format port "<word: ~a>" 
          (syl-ls word)))

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
  (ly (phrase <phrase>))
  (string-join (map ly (word-ls phrase)) " "))

(define-method
  (sxml-mei (phrase <phrase>))
  (map sxml-mei (word-ls phrase)))

(define-method
  (mei (phrase <phrase>))
  (sxml->xml (sxml-mei phrase)))

(define-method
  (write (phrase <phrase>) port)
  (format port "<phrase: ~a>\n" (word-ls phrase)))

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
    "Split string STR into a list of strings at occurences of token TOK (can be
    any character predicate) like string-tokenize, but leave TOK at the end of
    each string"
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


(define file->sentences
  (lambda (infile)
    "Read text from input file, remove blank lines and comment lines, 
    separate at newlines into list of sentence strings with whitespace removed."

    (define get-text
      (lambda (infile)
        (call-with-input-file infile get-string-all)))

    (define text->lines
      (lambda (str)
        (string-split str #\newline)))
    
    (define strip-whitespace-str
      (lambda (str) 
        (string-trim-both str char-set:whitespace)))

    (let* ([text (get-text infile)]
           [lines (text->lines text)]
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

    (define group-word-ls
      (lambda (ls)
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

(define file->arca
  (lambda (infile)
    "Convert text from INFILE to phrases in arca input format"
    (define Group-min 2)
    (define Group-max 6)

    (let* ([sentences (file->sentences infile)]
           [syllables (map sentence->syllables sentences)]
           [arca (word-groups->arca syllables Group-min Group-max)])
      arca)))

(define arca->mei
  (lambda (text)

    (define sentence->mei
      (lambda (ls)
        (fold
          (lambda (this acc) (cons (sxml-mei this) acc))
          '() ls)))

    (let ([body (map sentence->mei text)])
      (sxml->xml `(lyrics ,body)))))

(define arca->ly
  (lambda (text)
    
    (define sentence->ly
      (lambda (s)
        (ly (car s))))

    (string-join (map sentence->ly text))))

(define file->mei
  (lambda (infile)
    (arca->mei (file->arca infile))))

(define file->ly
  (lambda (infile)
    (arca->ly (file->arca infile))))
