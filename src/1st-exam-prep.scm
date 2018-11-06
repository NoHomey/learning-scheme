(define (number-to-list-of-digits n)
    (define (convert n l)
        (if (< n 10)
            (cons n l)
            (convert (quotient n 10) (cons (remainder n 10) l))
        )
    )
    (convert n '())
)

(define (middle-digit n)
    ((lambda (l)
        ((lambda (ln)
            (if (even? ln) -1 (list-ref l (quotient ln 2)))
        ) (length l))
    ) (number-to-list-of-digits n))
)

(define (every? l) (fold-left (lambda (u v) (and u v)) #t l))

(define (is-closed? l f)
    (define (member x)
        (define (rec-member l)
            (if (null? l) #f (if (= (car l) x) #t (rec-member (cdr l))))
        )
        (rec-member l)
    )
    (every? (map member (map f l)))
)

(define (is-hm? l op f)
    (every? (map (lambda (x)
        (every? (map (lambda (y)
            (= (op (f x) (f y)) (f (op x y)))
        ) l))
    ) l))
)

(define (is-em? l op f) (and (is-closed? l f) (is-hm? l op f)))

(define (accumulate-l op ne term next a b)
    (define (step ne c)
        (if (> c b) ne (step (op ne (term c)) (next c)))
    )
    (step ne a)
)

(define (meetTwice? f g a b)
    (> (accumulate-l + 0 (lambda (x)
        (if (= (f x) (g x)) 1 0)
    ) (lambda (i) (+ i 1)) a b) 1)
)

(define (group l)
    (define (run l)
        (if (null? (cdr l))
            (cons (cons (car l) 1) '())
            ((lambda (curr res)
                (if (= curr (car (car res)))
                    (cons (cons curr (+ 1 (cdr (car res)))) (cdr res))
                    (cons (cons curr 1) res)
                )
            ) (car l) (run (cdr l)))
        )
    )
    (if (null? l) l (run l))
)

(define (look-and-say l)
    (define (encode info)
        (if (null? info)
            info
            (cons
                (cdr (car info))
                (cons (car (car info)) (encode (cdr info)))        
            )
        )
    )
    (encode (group l))
)

; (load "1st-exam-prep.scm")