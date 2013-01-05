; EOPL exercise 1.34
;
; Write a procedure path that takes an integer n and a binary search tree bst
; (page 10) that contains the integer n, and returns a list of lefts and
; rights showing how to find the node containing n. If n is found at the root,
; it returns the empty list.
;
; > (path 17 '(14 (7 () (12 () ()))
;                 (26 (20 (17 () ())
;                         ())
;                     (31 () ()))))
; (right left left)

(define (path n bst)
  (if (eqv? n (car bst))
      '()
      (if (< n (car bst))
          (cons 'left (path n (cadr bst)))
          (cons 'right (path n (caddr bst))))))
