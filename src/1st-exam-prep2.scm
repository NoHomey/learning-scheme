(define (longest-descending-prefix l)
    (if (null? l)
        l
        (cons (car l) (cond
            ((null? (cdr l)) '())    
            ((> (car l) (car (cdr l))) (longest-descending-prefix (cdr l)))
            (else '())
        ))
    )
)

(define (longest-descending l)
    (define (max prev l)
        (if (null? l)
            prev
            ((lambda (curr)
                (max (if (> (length curr) (length prev)) curr prev) (cdr l))
            ) (longest-descending-prefix l))
        )
    )
    (max (longest-descending-prefix l) (cdr l))
)

(define (member? e l)
    (if (null? l) #f (if (eqv? (car l) e) #t (member? e (cdr l))))
)

(define (filter p? l)
    (cond
        ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))    
    )
)

(define (not-eqv-to? to) (lambda (x) (not (eqv? x to))))

(define (remove-all e l) (filter (not-eqv-to? e) l))

(define (uniques l)
    (if (null? l) l
        ((lambda (e ul)
            (if (member? e ul)
                (remove-all e ul)
                (cons e ul)
            )
        ) (car l) (uniques (cdr l)))
    )
)

(define (foldl op ne l)
    (if (null? l) ne (foldl op (op ne (car l)) (cdr l)))
)

(define (l-max l)
    (foldl (lambda (c n) (if (> n c) n c)) (car l) (cdr l))
)

(define (max-unique l)
    ((lambda (ul)
        (if (null? ul)
            #f
            (l-max (map l-max ul))
        )
    ) (filter (lambda (ml) (not (null? ml))) (map uniques l)))
)

(max-unique '((1 2 3 2) (5 5) (0))) 

; (load "src/1st-exam-prep2.scm")