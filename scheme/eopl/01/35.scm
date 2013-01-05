; EOPL exercise 1.35
;
; Write a procedure number-leaves that takes a bintree, and produces a bintree
; like the original, except the contents of the leaves are numbered starting
; from 0. For example,
;
; (number-leaves
;   (interior-node 'foo
;     (interior-node 'bar
;       (leaf 26)
;       (leaf 12))
;     (interior-node 'baz
;       (leaf 11)
;       (interior-node 'quux
;         (leaf 117)
;         (leaf 14)))))
;
; should return
;
; (foo
;   (bar 0 1)
;   (baz
;     2
;     (quux 3 4)))

(load-relative "31.scm")

(define (number-leaves tree)
  (define (traverse tree counter)
    (if (leaf? tree)
      (cons (+ counter 1) (leaf counter))
      (let* ((node-data (contents-of tree))
             (lson-result (traverse (lson tree) counter))
             (counter-after-lson (car lson-result))
             (result-lson (cdr lson-result))
             (rson-result (traverse (rson tree) counter-after-lson))
             (result-counter (car rson-result))
             (result-rson (cdr rson-result))
             (result-tree (interior-node node-data result-lson result-rson)))
        (cons result-counter result-tree))))
  (cdr (traverse tree 0)))
