; EOPL exercise 1.33
;
; Write a procedure mark-leaves-with-red-depth that takes a bintree
; (definition 1.1.7), and produces a bintree of the same shape as the
; original, except that in the new tree, each leaf contains the integer of
; nodes between it and the root that contain the symbol red. For example, the
; expression
;
; (mark-leaves-with-red-depth
;   (interior-node 'bar
;     (interior-node 'bar
;       (leaf 26)
;       (leaf 12))
;     (interior-node 'red
;       (leaf 11)
;       (interior-node 'quux
;         (leaf 117)
;         (leaf 14)))))
;
; which is written using the procedures defined in exercise 1.31, should
; return the bintree
;
; (red
;   (bar 1 1)
;   (red 2 (quux 2 2)))

(load-relative "31.scm")

(define (mark-leaves-with-red-depth tree)
  (define (traverse tree count)
    (if (leaf? tree)
      (leaf count)
      (let ((new-count (if (eqv? (contents-of tree) 'red)
                           (+ count 1)
                           count)))
        (interior-node (contents-of tree)
                       (traverse (lson tree) new-count)
                       (traverse (rson tree) new-count)))))
  (traverse tree 0))
