(define (rec-closure f start) (f start f))

(define (accumulate operation base start end term next)
    (rec-closure (lambda (current self)
        (if (> current end)
            base
            (operation (term current) (self (next current) self))
        )
    ) start)
)

(define (compose f g) (lambda (x) (f (g x))))

(define (apply-twice f x) ((compose f f) x))

(define (id x) x)

(define (adder i) (lambda (x) (+ x i)))

(define next (adder 1))

(define (const x) (lambda (y) x))

(define (f-pow f n) (accumulate compose id 1 n (const f) next))

(define (apply-n n f x) ((f-pow f n) x))

(define (factorial n) (accumulate * 1 1 n id next))

(define (double-factorial n) (accumulate * 1 1 n id (adder 2)))

(define (filter-accumulate-result p? i current rest operation)
    (if (p? i current) (operation current rest) rest)
)

(define (filter-accumulate operation base start end term next p?)
    (rec-closure (lambda (current self)
        (if (> current end)
            base
            (filter-accumulate-result p? current (term current) (self (next current) self) operation)
        )
    ) start)
)

(define (count p? a b) (filter-accumulate + 0 a b (const 1) next p?))

(define (sum-divisors n)
    (filter-accumulate + 0 1 n id next (lambda (i current) (= (remainder n i) 0)))
)