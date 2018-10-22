(define add (lambda (x) (lambda (y) (+ x y))))

(define suc (add 1))

(define (sum-of-divisors n)
    (define (include-in-sum d sum)
        (if (= d n)
            (+ sum n)
            (include-in-sum (suc d) ((add sum) (if (= (remainder n d) 0) d 0)))
        )
    )
    (include-in-sum 1 0)
)

(define (increasing-digits n)
    (letrec (
        (computation (lambda (p d)
            (cons (if (car p) (<= (cdr p) d) #f) d)
        ))
        (helper (lambda (n)
            (if (< n 10)
                (cons #t n)
                (computation (helper (quotient n 10)) (remainder n 10))
            )
        )))
        (car (helper n))
    )
)

(define (binary-to-decimal n)
    (define (helper n mul sum)
        (if (= n 0)
            sum
            (helper (quotient n 10) (* mul 2) (+ sum (* mul (remainder n 10))))    
        )
    )
    (helper n 1 0)
)

(define (number-to-list n list)
    (if (< n 10) (cons n list) (number-to-list (quotient n 10) (cons (remainder n 10) list)))
)

(define (prefix? l1 l2)
    (if (null? l1)
        #t
        (if (null? l2)
            #f
            (and (eq? (car l1) (car l2)) (prefix? (cdr l1) (cdr l2)))
        )
    )
)

(define (sublist? l1 l2)
    (if (null? l2) #f (or (prefix? l1 l2) (sublist? l1 (cdr l2))))
)

(define (contains? a b)
    (sublist? (number-to-list b '()) (number-to-list a '()))
)

(define (palindrome? n)
    (let ((list (number-to-list n '()))) (prefix? list (reverse list)))
)