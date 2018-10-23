(define (expr->tree e) e)

(define (symbol->func s)
    (cond
        ((eq? '+ s) +)
        ((eq? '- s) -)
        ((eq? '* s) *)
        ((eq? '/ s) /)
        ((eq? 'expt s) expt)
        (else s)
    )
)

(define (eval-tree t x)
    ((lambda (f t) (f f t)) (lambda (self t)
        (cond
            ((eq? 'x t) x)
            ((number? t) t)
            (else ((symbol->func (car t)) (self self (car (cdr t))) (self self (car (cdr (cdr t))))))
        )
    ) t)
)