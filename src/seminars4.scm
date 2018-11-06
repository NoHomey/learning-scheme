(define (zip l1 l2)
    (if (or (null? l1) (null? l2))
        '()
        (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))
    )
)

(define (append l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2))
    )
)

(define (flat l)
    (cond
        ((null? l) l)
        ((pair? l) (append (flat (car l)) (flat (cdr l))))
        (else (cons l '()))
    )
)

(define (sorted-insert l e)
    (define (insert l)
        (cond
            ((null? l) (cons e l))
            ((<= e (car l)) (cons e l))  
            (else (cons (car l) (insert (cdr l))))
        )
    )
    (insert l)
)

(define (foldr op ne l)
    (define (fold ne l)
        (if (null? l)
            ne
            (fold (op ne (car l)) (cdr l))
        )
    )
    (fold ne l)
)

(define (insertion-sort l) (foldr sorted-insert '() l))

(define (member? l)
    (define (find? e l)
        (if (null? l)
            #f
            (if (eqv? e (car l))
                #t
                (find? e (cdr l))
            )
        )
    )
    (lambda (e) (find? e l))
)

(define (filter f l)
    (cond
        ((null? l) l)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))
    )
)

(define (negate f) (lambda (x) (not (f x))))

(define (diff a b) (filter (negate (member? b)) a))

(define (intersection a b) (filter (member? b) a))

(define (union-of-elements a) (foldr append '() a))

(define (union a b) (union-of-elements (list (diff a b) (intersection a b) (diff b a))))

(define (subsets l)
    (if (null? l)
        (cons '() '())
        ((lambda (e subt)
            (append (map (lambda (sub) (cons e sub)) subt) subt)
        ) (car l) (subsets (cdr l)))
    )
)

(define (take n l)
    (define (rec k l)
        (if (= n k)
            '()
            (cons (car l) (rec (+ k 1) (cdr l)))
        )
    )
    (rec 0 l)
)

(define (k-sublists l n)
    (lambda (k)
        (define (rec c l)
            (if (= c n)
                (cons l '())
                (cons (take k l) (rec (+ c 1) (cdr l)))
            )
        )
        (rec k l)
    )
)

(define (first-n n)
    (define  (rec k res)
        (if (= 0 k) res (rec (- k 1) (cons k res)))
    )
    (rec n '())
)

(define (sublists l)
    ((lambda (n)
        (foldr append '(()) (map (k-sublists l n) (first-n n)))
    ) (length l))
)

(sublists '(1 2 3 4))

; (load "src/seminars4.scm")