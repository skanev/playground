; EOPL exercise 1.21
;
; (product sos1 sos2), where sos1 and sos2 are each a list of symbols without
; repetitions, returns a list of 2-lists that represents a Cartesian product
; of sos1 and sos2. The 2-lists can appear in any order.
;
; > (product '(a b c) '(x y))
; ((a x) (a y) (b x) (b y) (c x) (c y))

(define (product sos1 sos2)
  (if (null? sos1)
    '()
    (append (map (lambda (s) (list (car sos1) s)) sos2)
            (product (cdr sos1) sos2))))

