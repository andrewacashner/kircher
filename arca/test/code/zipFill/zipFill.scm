(use-modules
  (srfi srfi-1))
(define zipFill
(lambda (lsA lsB test sub)
    (if (= (length lsA) (length lsB))
        (zip lsA lsB)
        (let loop ([lsA lsA] [lsB lsB] [new '()])
            (if (or (null? lsA) (null? lsB))
                (reverse new)
                (let ([thisA (car lsA)]
                      [thisB (car lsB)])
                (if (test thisA)
                    (loop thisA lsB (cons (cons thisA sub) new))
                    (loop (cdr lsA) (cdr lsB) 
                         (cons (cons thisA thisB) new)))))))))

(let ([alpha '(a b c d a f g a h)]
      [num '(1 2 3 4 5 6 7)])
  (zipFill alpha num (lambda (x) (eq? 'a x)) 0))

