; EOPL exercise 1.32
;
; Write a procedure double-tree that takes a bintree, as represented in
; definition 1.1.7, and produces another bintree like the original, but with
; all the integers in the leaves doubled.

; We base this on the previous exercise
(load-relative "31.scm")

(define (double-tree tree)
  (if (leaf? tree)
    (* (contents-of tree) 2)
    (interior-node (contents-of tree)
                   (double-tree (lson tree))
                   (double-tree (rson tree)))))
