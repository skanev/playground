; EOPL exercise 2.25
;
; Use cases to write max-interior, which takes a binary tree of integers (as
; in the preceding exercise) with at least one interior node and returns the
; symbol associated with an interior node with a maximal leaf sum.
;
; > (define tree-1
;     (interior-node 'foo (leaf-node 2) (leaf-node 3)))
; > (define tree-2
;     (interior-node 'bar (leaf-node -1) tree-1))
; > (define tree-3
;     (interior-node 'baz tree-2 (leaf-node 1)))
; > (max-interior tree-2)
; foo
; > (max-interior tree-3)
; baz
;
; The last invocation of max-interior might also have returned foo, since both
; the foo and baz nodes have a leaf sum of 5.

(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

(define (key-of tree)
  (cases bintree tree
    (leaf-node (num) (eopl:error 'key-of "Leaf node ~s" num))
    (interior-node (key left right) key)))

(define (tree-sum tree)
  (cases bintree tree
    (leaf-node (num) num)
    (interior-node (key left right)
      (+ (tree-sum left) (tree-sum right)))))

(define (interior? tree)
  (cases bintree tree
    (leaf-node (num) #f)
    (interior-node (key left right) #t)))

(define (max-node tree)
  (cases bintree tree
    (leaf-node (num) #f)
    (interior-node (key left right)
      (let ((max-child (cond ((and (interior? left) (interior? right))
                              (if (< (tree-sum left) (tree-sum right))
                                  right
                                  left))
                             ((and (interior? left)) left)
                             ((and (interior? right)) right)
                             (else #f))))
        (cond ((not max-child) tree)
              ((< (tree-sum max-child) (tree-sum tree)) tree)
              (else max-child))))))

(define (max-interior tree)
  (key-of (max-node tree)))
