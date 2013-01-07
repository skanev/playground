; EOPL exercise 2.26
;
; Here is another version of exercise 1.33. Consider a set of trees given by
; the following grammar:
;
;   Red-blue-tree    ::= Red-blue-subtree
;   Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;                        (blue-node {Red-blue-subtree}*)
;                        (leaf-node Int)
;
; Write an equivalent definition using define-datatype, and use the resulting
; interface to write a procedure that takes a tree and builds a tree of the
; same shape, except that each leaf node is replaced by a leaf node that
; contains the number of red nodes on the path between it and the root.

(define-datatype red-blue-tree red-blue-tree?
  (red-node
    (left red-blue-tree?)
    (right red-blue-tree?))
  (blue-node
    (trees (list-of red-blue-tree?)))
  (leaf-node
    (num integer?)))

(define (list-of predicate)
  (lambda (object)
    (or (null? object)
        (and (pair? object)
             (predicate (car object))
             ((list-of predicate) (cdr object))))))

(define (mark-with-red-depth tree)
  (define (iter tree counter)
    (cases red-blue-tree tree
      (red-node (left right)
        (red-node (iter left (+ counter 1))
                  (iter right (+ counter 1))))
      (blue-node (nodes)
        (blue-node (map (curryr iter counter) nodes)))
      (leaf-node (num)
        (leaf-node counter))))
  (iter tree 0))
