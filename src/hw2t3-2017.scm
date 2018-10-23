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
    (cond
        ((eq? 'x t) x)
        ((number? t) t)
        (else ((symbol->func (car t)) (eval-tree (car (cdr t))) (eval-tree (car (cdr (cdr t))))))
    )
)

(define (tree-simplify? t)
    (split-tree (lambda (s f g)
        (cond
            ((or (eq? '+ s) (eq? '- s))
                (or
                    (and (number? f) (number? g))
                    (and (number? f) (= 0 f))
                    (and (number? g) (= 0 g))
                )
            )
            ((eq? '* s)
                (or
                    (and (number? f) (number? g))
                    (and (number? f) (or (= 0 f) (= 1 f)))
                    (and (number? g) (or (= 0 g) (= 1 g)))
                )
            )
            (else #f)
        )
    ) t)
)

(define (keep-tree-simplify t) (if (tree-simplify? t) (tree-simplify t) t))

(define (split-tree f t) (f (car t) (car (cdr t)) (car (cdr (cdr t)))))

(define (tree-simplify t)
    (if (list? t)
        (split-tree (lambda (s f g)
            (cond
                ((or (eq? '+ s) (eq? '- s))
                    (cond
                        ((and (number? f) (number? g) ((if (eq? '+ s) + -) f g)))
                        ((and (number? f) (= 0 f)) (tree-simplify g))
                        ((and (number? g) (= 0 g)) (tree-simplify f))
                        (else (keep-tree-simplify (list s (tree-simplify f) (tree-simplify g))))
                    )
                )
                ((eq? '* s) (cond
                    ((and (number? f) (number? g) (* f g)))
                    ((number? f)
                        (cond
                            ((= 0 f) 0)
                            ((= 1 f) (tree-simplify g))
                            (else (keep-tree-simplify (list s f (tree-simplify g))))
                        )
                    )
                    ((number? g)
                        (cond
                            ((= 0 g) 0)
                            ((= 1 g) (tree-simplify f))
                            (else (keep-tree-simplify (list s (tree-simplify f) g)))
                        )
                    )
                    (else (keep-tree-simplify (list s (tree-simplify f) (tree-simplify g))))
                ))
                (else t)
            )
        ) t)
    )
)

(define (tree-derive t)
    (cond
        ((eq? 'x t) 1)
        ((number? t) 0)
        (else (split-tree (lambda (s f g)
            (cond
                ((or (eq? '+ s) (eq? '- s)) (list s (tree-derive f) (tree-derive g)))
                ((eq? '* s) (list '+ (list '* (tree-derive f) g) (list '* f (tree-derive g))))
                ((eq? '/ s) (list '/ (list '+ (list '* (tree-derive f) g) (list '* f (tree-derive g))) (list '* g g)))
                (else #f)
            )
        ) t))
    )
)