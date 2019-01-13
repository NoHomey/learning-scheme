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

; solution

(define (insertUniqueIntoSorted v s)
    (define (insert sl)
        (cond 
            ((null? sl) (cons v '()))   
            ((< (car sl) v) (cons (car sl) (insert (cdr sl))))
            ((> (car sl) v) (cons v sl))
            (else sl)
        )
    )
    (insert s)
)

(define root-tree car)

(define childs-tree cdr)

(define (equal-lists l1 l2)
    (if (and (null? l1) (null? l2))
        #t
        (if (and (not (null? l1)) (not (null? l2)))
            (and (= (car l1) (car l2)) (equal-lists (cdr l1) (cdr l2)))
            #f
        )
    )
)

(define (flip f) (lambda (u v) (f v u)))

(define (family-set t)
    (foldl (flip insertUniqueIntoSorted) (cons (root-tree t) '()) (map root-tree (childs-tree t)))
)

(define (similarFamilies t1 t2)
    (equal-lists (family-set t1) (family-set t2))
)

(define build-tree cons)

(define (leaf-tree r) (build-tree r '()))

(define (similarButDistant t)
    (define (helper r tc)
        (if (eq? r tc)
            #f
            (if (similarFamilies r tc)
                #t
                (any (lambda (u) (similarButDistant r u)) (childs-tree tc))
            )
        )
    )
    (any (lambda (v) (any (lambda (u) (helper v u)) (childs-tree t))) (childs-tree t))
)

; test
;(similarFamilies (build-tree 4 (list (leaf-tree 2) (leaf-tree 1) (leaf-tree 2))) (build-tree 1 (list (leaf-tree 4) (leaf-tree 2))))
;(similarFamilies (build-tree 4 (list (leaf-tree 2) (leaf-tree 1) (leaf-tree 3))) (build-tree 1 (list (leaf-tree 4) (leaf-tree 2))))
;(similarButDistant (build-tree 4 (list (leaf-tree 2) (leaf-tree 1) (leaf-tree 2))))
;(similarButDistant (build-tree 4 (list (leaf-tree 2) (leaf-tree 1) (leaf-tree 3))))
