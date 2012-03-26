; SICP exercise 2.31
;
; Abstract your answers to exercise 2.30 to produce a procedure tree-map with
; the property that square-tree could be defined as
;
; (define (square-tree tree) (tree-map square tree))

(define (tree-map function tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (function tree))
        (else (cons (tree-map function (car tree))
                    (tree-map function (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))

(define (square x)
  (* x x))
