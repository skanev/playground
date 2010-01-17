; Exercise 2.24
(module bintree-to-list eopl
  (provide interior-node leaf-node bintree-to-list)

  (define-datatype bintree bintree?
    (leaf-node
      (num integer?))
    (interior-node
      (key symbol?)
      (left bintree?)
      (right bintree?)))

  (define bintree-to-list
    (lambda (tree)
      (cases bintree tree
        (leaf-node (num) (list 'leaf-node num))
        (interior-node (key left right) (list 'interior-node key (bintree-to-list left) (bintree-to-list right))))))
)
