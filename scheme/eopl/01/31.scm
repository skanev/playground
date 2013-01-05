; EOPL exercise 1.31
;
; Write the following procedures for calculating on a bintree (definition
; 1.1.7): leaf and interior-node, which builds bintrees, leaf?, which tests
; whether a bintree is a leaf, and lson, rson and contents-of, which extract
; components of a node. contents-of should work on both leaves end interior
; nodes.

(define (leaf? tree)
  (number? tree))

(define (leaf number)
  (if (number? number)
    number
    (eopl:error 'leaf "Leaf argument must be a number. Got ~s instead." number)))

(define (interior-node data left-son right-son)
  (if (symbol? data)
    (list data left-son right-son)
    (eopl:error 'interior-node "Data must be a symbol. Got ~s instead." data)))

(define (lson tree) (cadr tree))
(define (rson tree) (caddr tree))

(define (contents-of tree)
  (if (leaf? tree)
    tree
    (car tree)))
