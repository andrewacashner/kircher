(define first-group
  (lambda (ls max)
    (let loop ([ls ls] [new '()])
      (if (null? ls)
        new
        (let ([next (cons (car ls) new)])
          (if (<= (apply + (map length next)) max)
            (loop (cdr ls) next)
            (loop '() (cons (reverse new) ls))))))))


(define regroup
  (lambda (ls max)
    (let loop ([ls ls] [new '()])
      (if (null? ls)
        (reverse new)
        (if (<= (apply + (map length ls)) max)
          (loop '() (cons ls new))
          (let ([this (first-group ls max)])
            (loop (cdr this) (cons (car this) new))))))))


