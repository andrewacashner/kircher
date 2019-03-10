; lectio.scm
; Andrew A. Cashner
; 2019-03-07
; Read and process text input for Kircher's Arca musarithmica

; TODO 
; - process list of lists of strings in obj #:text to arca input and MEI or
;   Lilypond code
; - make objects for sentences, words, syllables if needed
; - make object for list of arca-text objects?

(use-modules
  (ice-9 format)
  (rnrs io ports)
  (srfi srfi-1)
  (oop goops))

;; DATA OBJECTS
(define-class
  <arca-word> ()
  (syl-count 
    #:init-value 0
    #:init-keyword #:syl-count
    #:getter syl-count)
  (accent
    #:init-value 0
    #:init-keyword #:accent
    #:getter accent)
  (syl-ls
    #:init-value '()
    #:init-keyword #:syl-ls
    #:getter syl-ls))

(define-class 
  <arca-phrase> ()
  (syl-count
    #:init-keyword #:syl-count
;    #:slot-set! (lambda (obj) 
;                  (apply + (map syl-count (word-ls obj))))
    #:getter syl-count)
  
  (penult-type ; calculate from last accent in word-ls
    #:init-keyword #:penult-type
;    #:slot-set! (lambda (obj) 
;                  (penult-type (word-ls obj)))
    #:getter penult-type)

  (word-ls
    #:init-value '() ; list of <arca-word> objects
    #:init-keyword #:word-ls
    #:getter word-ls))


(define-method 
  (write (phrase <arca-phrase>) port)
  (let ([word-ls (word-ls phrase)])
    (format port "~d ~c ~a\n" 
            (syl-count phrase)
            (if (eq? (penult-type obj) 'long) #\L #\S)
            (map (lambda (word port) (write word port)) word-ls))))


(define-method
  (write (word <arca-word>) port) 
  (let* ([ls (syl-ls word)]
         [text (string-join ls " -- ")])
    (format port "~s\n" text)))


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

; TODO or you could preserve accents and match them with rperms accordingly!

(define word-groups->arca
  (lambda (ls shortest longest)
    "Given a list of word groups, return a list of structures with syllable
    count, penultimate syllable quantity, and text for each group"

    (define last-long-syl
      (lambda (ls)
        ; "Given list of syllable strings with accent marks on long syllables,
        ; return the position of the last long syllable in the phrase counting
        ; back from the end"
        (let ([ls (last ls)])
          (let loop ([ls (reverse ls)] [count 0])
            (if (or (null? ls)
                    (char=? (string-ref (car ls) 0) #\')) ; long syllable?
                count
                (loop (cdr ls) (+ 1 count)))))))

      (define penult-type
        (lambda (ls)
          (if (= 1 (last-long-syl ls)) 'long 'short)))

      (define remove-accents
        (lambda (ls)
          (map (lambda (str) (string-delete #\' str)) ls)))

      (define group-word-ls
        (lambda (ls)
          (group-words ls shortest longest)))

      (define arca-phrase
        (lambda (ls)
          (make <arca-phrase> 
                #:syl-count (apply + (map length ls))
                #:penult-type (penult-type ls)
                #:word-ls (map remove-accents ls))))

      (let ([groups (map group-word-ls ls)]) 
        (fold-right 
          (lambda (this acc)
            (cons (map arca-phrase this) acc))
          '()
          groups))))

(define file->arca
  (lambda (infile)
    "Convert text from INFILE to phrases in arca input format"
    (define Group-min 2)
    (define Group-max 6)

    (define get-word-groups
      (lambda (ls)
        (group-words ls Group-min Group-max)))

    (define group->arca
      (lambda (ls)
        (group-words ls Group-min Group-max)))

    (let* ([sentences (file->sentences infile)]
           [syllables (map sentence->syllables sentences)]
           [groups (map get-word-groups syllables)]
           [arca (map group->arca groups)])
      arca)))


