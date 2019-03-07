; lectio.scm
; Andrew A. Cashner
; 2019-03-07
; Read and process text input for Kircher's Arca musarithmica

(use-modules
  (rnrs io ports)
  (srfi srfi-1))

(define get-text
  (lambda (infile)
    (call-with-input-file infile get-string-all)))

(define text->lines
  (lambda (str)
    (string-split str #\newline)))

(define not-comment?
  (lambda (str)
    "Boolean: Does the string STR not start with comment char?"
    (not (char=? (string-ref str 0) #\%)))) 

(define str-not-blank?
  (lambda (str)
    "Boolean: Is the string STR not blank?"
    (not (= (string-length str) 0))))

(define strip-blank-lines
  (lambda (ls)
    "Given list of strings, remove blank lines"
    (filter str-not-blank? ls)))

(define strip-comments
  (lambda (ls)
    "Given list of strings, remove all beginning with comment char"
    (filter not-comment? ls)))

(define string-tokenize-keep-token
  (lambda (str tok)
    "Split string STR into a list of strings at occurences of token TOK, 
    like string-tokenize, but leave TOK at the end of each string"
    (let loop ([str str] [ls '()])
      (if (= 0 (string-length str))
          (reverse ls)
          (let ([first-tok (string-index str tok)])
            (if (eq? #f first-tok)
                (loop "" (cons str ls))
                (let* ([split-index (+ 1 first-tok)]
                       [select (substring str 0 split-index)]
                       [tail (string-drop str split-index)])
                  (loop tail (cons select ls)))))))))

(define strip-whitespace
  (lambda (ls)
    "Strip leading and trailing whitespace from all strings in a list"
    (let ([strip (lambda (str) 
                   (string-trim-both str char-set:whitespace))])
      (map strip ls))))

(define lines->sentences
  (lambda (ls)
    "Given list of strings originally separated by newlines,
    merge the strings and then make a new list of strings separated by sentences
    instead"
    (let* ([text (string-join ls)]
           [tokenized (string-tokenize-keep-token text #\.)])
      (strip-whitespace tokenized))))

; TODO use character set including #\? or other delimiters instead of the single
; one

(define file->sentences
  (lambda (infile)
    "Read text from input file INFILE with syllables separated by hyphens; 
    return a list of strings, one per syllable"
    (let* ([text (get-text infile)]
           [lines (text->lines text)]
           [no-blanks (strip-blank-lines lines)]
           [no-comments (strip-comments no-blanks)]) 
      (lines->sentences no-comments))))

(define split-words
  (lambda (str)
    "Split string STR into list of words"
    (string-split str char-set:whitespace)))

(define split-syllables
  (lambda (str)
    "Split string STR with syllables separated by hyphens into list of strings,
    one per syllable"
    (string-split str #\-)))

(define sentence->syllables
  (lambda (str)
    "Given sentence as string STR, return a list of words, where each word is a
    sub-list of its syllables"
    (let ([words (split-words str)])
      (map split-syllables words))))

(define file->syllables
  (lambda (infile)
    "Read text from INFILE and return a list of sentences, where each sentence
    is a list of words, where each word is a list of syllables."
    (let ([text (file->sentences infile)])
      (map sentence->syllables text))))

(define count-syllables
  (lambda (str)
    "Count syllables in string divided by hyphens"
    (let ([ls (split-syllables str)])
      (length ls))))

(define syllables-per-word
  (lambda (ls)
    "Given a list containing a sentence divided into words and syllables
    (using sentence->syllables), return a list of the syllable counts for each
    word"
    (fold-right 
      (lambda (sub-ls acc)
            (cons (length sub-ls) acc))
      '()
      ls)))

(define divide-sentence
  (lambda (ls)
    "Given list of syllable-divided words in a sentence, divide the sentence
    into an optimal number of groups of syllables where 2 <= syl <= 6"
    (let loop ([old ls] [new '()])
      (if (null? old)
          (reverse new)
          (let* ([group (make-group old 6)]
                 [tail (lset-difference eq? old group)])
            (loop tail (cons group new)))))))

(define make-group
  (lambda (ls max)
    "Return the first subset of ls with a length <= max"
    (let loop ([ls ls] [group '()])
      (if (or (null? ls)
              (= (length group) max))
          (reverse group)
          (loop (cdr ls) (cons (car ls) group))))))

; TODO this works for integers, to make it work for syllable lists,
; need to make group based on syllable counts and use something other than
; lset-difference to get the tail (e.g. track length of groups extracted from ls
; and use list-ref)

; TODO convert divided sentences into appropriate output format to interface
; with arca; mark phrases with quantity of penultimate syllable
