(define (zip l1 l2)
    (if (or (null? l1) (null? l2))
        '()
        (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))
    )
)

(define (foldl op ne l)
    (if (null? l) ne (foldl op (op ne (car l)) (cdr l)))
)

(define (m-sum m)
    (foldl + 0 (map (lambda (row) (foldl + 0 row)) m))
)

(define (first-n n)
    (define (rec k res)
        (if (= k 0) res (rec (- k 1 ) (cons (- k 1) res)))
    )
    (rec n '())
)

(define (m-e m i j) (list-ref (list-ref m i) j))

(define m-rows length)

(define (m-cols m) (length (car m)))

(define (m-row m r) (list-ref m r))

(define (m-col m c) (map (lambda (row) (list-ref row c)) m))

(define (m-diag m)
    (map (lambda (i) (m-e m i i)) (first-n (m-rows m)))
)

(define (m-transpose m) (map (lambda (c) (m-col m c)) (first-n (m-cols m))))

(define (id x) x)

(define (m-set m i j x)
    (map (lambda (r)
        ((if (= r i)
            (lambda (row) (map (lambda (c)
                (if (= c j) x (list-ref row c))
            ) (first-n (length row))))
            id
        ) (m-row m r))
    ) (first-n (m-rows m)))
)

(define (sum-m a b)
    (map (lambda (rows)
        (map (lambda (p)
            (+ (car p) (cdr p))
        ) (zip (car rows) (cdr rows)))
    ) (zip a b))
)

(define matrix '((1 2 3)
                 (4 5 6)
                 (7 8 9)))

(sum-m '((1 2 3) (4 5 6) (7 8 9)) '((0 1 2) (3 4 5) (6 7 8)))

; (load "src/seminars5.scm")