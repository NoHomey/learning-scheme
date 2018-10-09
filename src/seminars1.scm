(define (even? number) (= (remainder number 2) 0))
 
(define (factorial number)
    (if (> number 1)
        (* number (factorial (- number 1)))
        1 
    )
)
 
(define (sum start end)
    (if (not (= start end))
        (+ start (sum (+ start 1) end))
        start
    )
)
 
(define (power a b)
    (if (not (= b 0))
        (* (power (* a a) (quotient b 2)) (if (= (remainder b 2) 0) 1 a))
        1
    )    
)
 
(define (gcd a b)
    (if (not (= b 0))
        ( gcd b (remainder a b))
        a   
    )    
)

(define (reverse number)
    (define (log10 number)
        (if (> number 9)
            (+ (log10 (quotient number 10)) 1)
            0
        )
    )
    (define (reverse-helper number multiplier)
        (if (> number 9)
            (+ (* (remainder number 10) multiplier) (reverse-helper (quotient number 10) (quotient multiplier 10)))
            number
        )
    )
    (reverse-helper number (power 10 (log10 number)))
)