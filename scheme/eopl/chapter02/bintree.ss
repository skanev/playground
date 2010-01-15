; Exercise 2.19
(module bintree eopl
  (provide number->bintree
           current-element move-to-left-son move-to-right-son at-leaf?
           insert-to-right insert-to-left)

  (define number->bintree (lambda (n) (list n '() '())))
  (define current-element (lambda (tree) (car tree)))
  (define move-to-left-son (lambda (tree) (cadr tree)))
  (define move-to-right-son (lambda (tree) (caddr tree)))
  (define at-leaf? (lambda (tree) (null? tree)))
  (define insert-to-left
    (lambda (n tree)
      (list
        (current-element tree)
        (list n (move-to-left-son tree) '())
        (move-to-right-son tree))
  ))
  (define insert-to-right
    (lambda (n tree)
      (list
        (current-element tree)
        (move-to-left-son tree)
        (list n (move-to-right-son tree) '()))
  ))
)
