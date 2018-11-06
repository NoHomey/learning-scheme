(define (foldr op ne l)
    (if (null? l)
        ne
        (op (car l) (foldr op ne (cdr l)))
    )
)

(define (foldl op ne l)
    (if (null? l)
        ne
        (foldl op (op ne (car l)) (cdr l))
    )
)

(define (map f l) (foldr (lambda (u v) (cons (f u) v)) '() l))

(define (filter p? l)
    (cond
        ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l)))) 
        (else (filter p? (cdr l)))
    )
)

(define (from-to a b next)
    (if (> a b)
        '()
        (cons a (from-to (next a) b next))
    )
)

(define (accumulate-r op ne term next a b)
    (foldr (lambda (u v) (op (term u) v)) ne (from-to a b next))
)

(define (accumulate-l op ne term next a b)
    (foldl (lambda (u v) (op u (term v))) ne (from-to a b next))
)

; solutions

(define (1+ i) (+ i 1))

; task 1. a)

(define (num-to-list n)
    (define (rec n l)
        (if (< n 10)
            (cons n l)
            (rec (quotient n 10) (cons (remainder n 10) l))
        )
    )
    (rec n '())
)

(define (narcissistic? num)
    ((lambda (l)
        ((lambda (n)
            (= num (foldl (lambda (acc c) (+ acc (expt c n))) 0 l))
        ) (length l))
    ) (num-to-list num))
)

; task 1. b)

(define (z-interval a b) (from-to a b 1+))

(define (d n)
    (foldl + 0 (filter (lambda (x) (= 0 (remainder n x))) (z-interval 1 (- n 1))))
)

(define (friendly? a b) (and (= (d a) b) (= (d b) a)))

; task 2.

(define (l-max l)
    (foldl (lambda (u v) (if (> v u) v u)) (car l) (cdr l))
)

(define (gen-intervals a b)
    (foldl append '()
        (map (lambda (s)
            (filter (lambda (p)
                (not (= (car p) (cdr p)))
            ) (map (lambda (f)
                (cons s f)
            ) (z-interval s b)))
        ) (z-interval a b))
    )
)

(define (calc-rec-interval f i j)
    (if (= i j) j (f i (calc-rec-interval f (1+ i) j)))
)

(define (calc-in-all-subintervals f a b)
    (map (lambda (p)
        (calc-rec-interval f (car p) (cdr p))
    ) (gen-intervals a b))
)

(define (findMax f a b) (l-max (calc-in-all-subintervals f a b)))

; task 3.

(define (open-interval-length i) (- (cdr i) (car i)))

(define (l-min f l)
    (foldl (lambda (u v) (if (< (f v) (f u)) v u)) (car l) (cdr l))
)

(define (shortest-length-open-interval l)
    (cdr (l-min car (map (lambda (i) (cons (open-interval-length i) i)) l)))
)

(define (subinterval i1 i2)
    (and (<= (car i2) (car i1)) (>= (cdr i2) (cdr i1)))
)

(define (sorted-insert order? e sl)
    (define (rec sl)
        (if (null? sl)
            (cons e sl)
            (if (order? (car sl) e)
                (cons (car sl) (rec (cdr sl)))
                (cons e sl)    
            )    
        )
    )
    (rec sl)
)

(define (rfoldl op ne l)
    (if (null? l)
        ne
        (rfoldl op (op (car l) ne) (cdr l))
    )
)

(define (insertion-sort order? l)
    (rfoldl (lambda (e csl) (sorted-insert order? e csl)) '() l)
)

(define (shortest-interval-supersets il)
    (insertion-sort (lambda (i1 i2) (< (cdr i1) (cdr i2))) ((lambda (si)
        (filter (lambda (i) (subinterval si i)) il)
    ) (shortest-length-open-interval il)))
)

; test

(define intervals '((90 . 110) (0 . 100) (24 . 26) (10 . 89) (1 . 5) (-4 . 25)))

(shortest-interval-supersets intervals)