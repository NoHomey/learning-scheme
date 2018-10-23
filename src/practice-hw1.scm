(define (f n) (if (even? n) (quotient n 2) (+ (* 3 n) 1)))

(define (stopping-time n)
    (define (helper n time)
        (if (= (f n) 1) time (helper (f n) (+ time 1)))
    )
    (helper n 1)
)

(define (from-ary-to-ary from to)
    (define (helper n m res)
        (if (= n 0) res (helper (quotient n to) (* m from) (+ res (* m (remainder n to))))) 
    )
    (lambda (n) (helper n 1 0))
)

(define (to-k-ary n k) ((from-ary-to-ary 10 k) n))

(define (from-k-ary n k) ((from-ary-to-ary k 10) n))

(define (x2 x) (* x x))

(define epsilon 1/1000000)

(define (find-initial-point y)
    (define (check? x) (and (> (x2 (+ x epsilon)) y) (< (x2 (- x epsilon)) y) ))
    (define (binary-search start end)
        ((lambda (mid)
            (if (check? mid)
                mid
                (if (< (x2 mid) y) (binary-search mid end) (binary-search start mid))   
            )
        ) (+ start (/ (- end start) 2)))
    )
    (binary-search 0 y)
)

(define (float-eq? x y) (or (< x (+ y epsilon)) (> x (- y epsilon))))

(define (root2 x)
    (define (next xn) (/ (+ xn (/ x xn)) 2))
    (define (iterate xp xc)
        (if (float-eq? xp xc) xc (iterate xc (next xc))) 
    )
    ((lambda (x0) (iterate x0 (next x0))) (find-initial-point x))
)

(define (arg-ext condition)
    (lambda (f l)
        (if (null? l)
            #f
            ((lambda (cache)
                (car ((lambda (ext val l) (ext ext val l))
                    (lambda (self ext l)
                        (if (null? l)
                            ext
                            (self self (if (condition (f (car l)) (cdr ext)) (cache (car l)) ext) (cdr l))
                        )
                    )
                    (cache (car l))
                    (cdr l)
                ))
            )(lambda (x) (cons x (f x))))
        )
    )
)

(define argmax (arg-ext >))

(define argmin (arg-ext <))