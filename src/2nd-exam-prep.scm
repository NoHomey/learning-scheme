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

(define (plus . nums) (foldl + 0 nums))

(define empty-tree? null?)

(define root-tree car)

(define left-tree cadr)

(define right-tree caddr)

(define (build-tree root left right) (list root left right))

(define (leaf root) (build-tree root '() '()))

(define (sum t)
    (if (empty-tree? t)
        0
        (+ (root-tree t) (sum (left-tree t)) (sum (right-tree t)))
    )
)

(define (map-tree f t)
    (define (rec t)
        (if (empty-tree? t)
            t
            (build-tree (f (root-tree t)) (rec (left-tree t)) (rec (right-tree t)))            
        )
    )
    (rec t)
)

(define example-tree (build-tree 1 (leaf 2) (build-tree 3 (leaf 4) (leaf 5))))

(sum example-tree)

(sum (map-tree (lambda (r) (* 2 r)) example-tree))

; (load "src/2nd-exam-prep.scm")