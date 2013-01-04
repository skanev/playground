; EOPL exercise 1.26
;
; (up lst) removes a pair of parentheses from each top-level element of lst.
; If a top-level element is not a list, it is included in result, as is. The
; value of (up (down lst)) is equivalent to lst, but (down (up lst)) is not
; necessarily lst. (See exercise 1.17).
;
; > (up '((1 2) (3 4)))
; (1 2 3 4)
; > (up '((x (y)) z))
; (x (y) z)

(define (up lst)
  (if (null? lst)
    '()
    (append (if (pair? (car lst))
              (car lst)
              (list (car lst)))
            (up (cdr lst)))))

