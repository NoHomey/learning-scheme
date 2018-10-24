(define (expr->ext e) e)

(define (symbol->func s)
    (cond
        ((eqv? '+ s) +)
        ((eqv? '- s) -)
        ((eqv? '* s) *)
        ((eqv? '/ s) /)
        ((eqv? 'expt s) expt)
        (else s)
    )
)

(define (split-ext f t)
    (f (car t) (car (cdr t)) (car (cdr (cdr t))))
)

(define (eval-ext t x)
    (cond
        ((eqv? 'x t) x)
        ((number? t) t)
        (else 
            (split-ext (lambda (s f g) 
               ((symbol->func s) (eval-ext f x) (eval-ext g x)) 
            ) t)    
        )
    )
)

(define (ext-op-normal-form t)
    (if (list? t)
        (split-ext (lambda (s f g)
            (cond
                ((eqv? '- s) (list '+ (ext-op-normal-form f) (if (number? g) (- g) (list '* -1 (ext-op-normal-form g)))))
                ((eqv? '-/ s) (list '* (ext-op-normal-form f) (if (number? g) (/ 1 g) (list '/ 1 (ext-op-normal-form g)))))
                (else (list s (ext-op-normal-form f) (ext-op-normal-form g)))
            )
        ) t)
        t
    )
)

(define (ext-number-normal-form t)
    (if (list? t)
        (split-ext (lambda (s f g) (
            (lambda (f g) 
                (if (and (number? f) (number? g))
                    ((symbol->func s) f g)
                    (if (or (eqv? 'expt s) (not (number? g)))
                        (list s f g)
                        (list s g f)
                    ) 
                )
            ) (ext-number-normal-form f) (ext-number-normal-form g))
        ) t)
        t
    )
)

(define (eqv-ext? t1 t2)
    (cond
        ((and (number? t1) (number? t2)) (eqv? t1 t2))
        ((and (list? t1) (list? t2))
            (split-ext (lambda (s1 f1 g1)
                (split-ext (lambda (s2 f2 g2)
                    (and (eqv? s1 s2) (or
                        (and (eqv-ext? f1 f2) (eqv-ext? g1 g2))
                        (and (eqv-ext? f1 g2) (eqv-ext? g1 f2))
                    ))
                ) t2)
            ) t1)       
        )
        (else (eqv? t1 t2))    
    )
)

(define (match f v)
    (if (and (not (null? v)) (not (null? f)))
        (if ((car f) (car v))
            (match (cdr f) (cdr v))
            #f
        )
        (and (null? f) (null? v))
    )
)

(define (ensure-list v) (if (list? v) v (cons v '())))

(define (eqv-to? to) (lambda (v) (eqv? to v)))

(define (eqv-to-ext? to) (lambda (v) (eqv-ext? to v)))

(define (any? v) #t)

(define (compose f g) (lambda (x) (f (g x))))

(define (eqv3? a b c) (and (eqv? a b) (eqv? a c)))

(define (simplify-commutative s f g)
    ((lambda (h f g) (if (number? g) (h g f) (h f g))) (lambda (f g)
        (cond
            ((match (list number? (eqv-to? s) number? any?) (cons f (ensure-list g)))
                (split-ext (lambda (s num t2)
                    (ext-simplify-number-after-op-normal-form (list s ((symbol->func s) f num) t2))
                ) g)
            )
            ((match (list (compose not number?) (eqv-to? s) number? any?) (cons f (ensure-list g)))
                (split-ext (lambda (s num t2)
                    (ext-simplify-number-after-op-normal-form (list s num (list s t2 f)))
                ) g)
            )
            ((eqv? '+ s)
                (cond
                    ((and (number? f) (= 0 f)) g)
                    ((eqv-ext? f g)
                        (ext-simplify-number-after-op-normal-form (list '* 2 f))
                    )
                    ((and (list? f) (list? g) (eqv3? (car f) (car g) '*))
                        (split-ext (lambda (m f1 g1)
                            (split-ext (lambda (m f2 g2)
                                (cond
                                    ((eqv-ext? f1 f2) (ext-simplify-number-after-op-normal-form (list '* f1 (list '+ g1 g2))))
                                    ((eqv-ext? f1 g2) (ext-simplify-number-after-op-normal-form (list '* f1 (list '+ g1 f2))))
                                    ((eqv-ext? g1 g2) (ext-simplify-number-after-op-normal-form (list '* g1 (list '+ f1 f2))))
                                    (else (list s f g)) 
                                )
                            ) g)
                        ) f)
                    )
                    ((match (list (eqv-to? '*) (eqv-to? -1) (eqv-to-ext? f)) (ensure-list g)) 0)
                    ((match (list (eqv-to? '*) any? (eqv-to-ext? g)) (ensure-list f))
                        (split-ext (lambda (m f1 ge)
                            (ext-simplify-number-after-op-normal-form (list '* (list '+ f1 1) g))
                        ) f)
                    )
                    ((match (list (eqv-to? '*) any? (eqv-to-ext? f)) (ensure-list g))
                        (split-ext (lambda (m f1 fe)
                            (ext-simplify-number-after-op-normal-form (list '* (list '+ f1 1) f))
                        ) g)
                    )
                    (else (list s f g))
                )
            )
            ((eqv? '* s)
                (cond
                    ((eqv-ext? f g)
                        (ext-simplify-number-after-op-normal-form (list 'expt f 2))
                    )
                    ((number? f)
                        (cond
                            ((= 0 f) 0)
                            ((= 1 f) g)
                            (else (list s f g))    
                        )
                    )
                    ((match (list (eqv-to? 'expt) (eqv-to-ext? g) any?) (ensure-list f))
                        (split-ext (lambda (s1 ge g1)
                            (ext-simplify-number-after-op-normal-form (list 'expt g (list '+ g1 1)))
                        ) f)
                    )
                    ((match (list (eqv-to? 'expt) (eqv-to-ext? f) any?) (ensure-list g))
                        (split-ext (lambda (s1 fe g1)
                            (ext-simplify-number-after-op-normal-form (list 'expt f (list '+ g1 1)))
                        ) g)
                    )
                    ((and (list? f) (list? g) (eqv3? (car f) (car g) 'expt) (eqv-ext? (car (cdr f)) (car (cdr g))))
                        (ext-simplify-number-after-op-normal-form
                            (list 'expt (car (cdr f)) (list '+ (list-ref f 2) (list-ref g 2)))
                        ) 
                    )
                    (else (list s f g))
                )
            )
            (else (list s f g))
        )
    ) f g)
)

(define (simplify-none-commutative s f g)
    (cond
        ((eqv? 'expt s)
            (cond
                ((and (number? g) (= 1 g)) f)
                ((and (number? g) (= 0 g)) 1)
                ((and (list? f) (eqv? 'expt (car f)))
                    (split-ext (lambda (s1 f1 g1)
                        (ext-simplify-number-after-op-normal-form (list s f1 (list '* g1 g)))
                    ) f)
                )
                (else (list s f g))
            )
        )
        (else (list s f g))    
    )
)

(define (is-commutative-op? op) (or (eqv? '+ op) (eqv? '* op)))

(define (ext-simplify-number-after-op-normal-form t)
    (if (list? t)
        (split-ext (lambda (s f g)
            (if (and (number? f) (number? g))
                ((symbol->func s) f g)
                ((if (is-commutative-op? s) simplify-commutative simplify-none-commutative)
                    s
                    (ext-simplify-number-after-op-normal-form f)
                    (ext-simplify-number-after-op-normal-form g)
                )
            )
        ) t)
        t
    )
)

(define (ext-simplify t)
    (ext-simplify-number-after-op-normal-form (ext-number-normal-form (ext-op-normal-form t)))
)

(define (direct-ext-derive t)
    (cond
        ((eqv? 'x t) 1)
        ((number? t) 0)
        (else (split-ext (lambda (s f g)
            (cond
                ((or (eqv? '+ s) (eqv? '- s)) (list s (direct-ext-derive f) (direct-ext-derive g)))
                ((eqv? '* s) (list '+ (list '* (direct-ext-derive f) g) (list '* f (direct-ext-derive g))))
                ((eqv? '/ s) (list '/ (list '+ (list '* (direct-ext-derive f) g) (list '* f (direct-ext-derive g))) (list '* g g)))
                ((and (eqv? 'expt s) (number? g))
                    (list '* g (list '* (list 'expt f (- g 1)) (direct-ext-derive f)))
                )
                (else #f)
            )
        ) t))
    )
)

(define (ext-derive t) (ext-simplify (direct-ext-derive (ext-simplify t))))

(ext-derive '(* (* (* x (* x 2)) 3) 2))

(ext-simplify '(* (* x 2) (* (* x 1) (* (* x 3) (* x 5)))))

(ext-simplify '(* (expt x 2) (expt x 3)))

(ext-simplify '(* (expt x -2) (expt x 3)))

(ext-simplify '(* (* (expt x -2) (* x x)) (expt x 3)))

(ext-simplify '(- x (* x -1)))

(ext-simplify '(expt (expt x (+ x 1)) (* 2 x)))

(ext-simplify '(+ (+ x 2) (+ (* x x) (- x (+ x 2)))))

; (load "hw2t3-2017.scm")