; Exercise 2.20
(module focused-bintree eopl
  (provide number->bintree
           current-element at-root? at-leaf?
           insert-to-left insert-to-right
           move-to-left move-to-right move-up)

  (define val car)
  (define left cadr)
  (define right caddr)
  (define top cadddr)

  (define number->bintree (lambda (n) (list n '() '() '())))
  (define current-element (lambda (tree) (val tree)))
  (define at-root? (lambda (tree) (null? (top tree))))
  (define at-leaf? (lambda (tree) (null? (val tree))))
  (define insert-to-left (lambda (n tree) (list (val tree) (list n (left tree) '()) (right tree) (top tree))))
  (define insert-to-right (lambda (n tree) (list (val tree) (left tree) (list n (right tree) '()) (top tree))))
  (define move-to-left
    (lambda (tree)
      (let [(child (left tree))]
        (list (val child) (left child) (right child) (list (val tree) 'left (right tree) (top tree))))))
  (define move-to-right
    (lambda (tree)
      (let [(child (right tree))]
        (list (val child) (left child) (right child) (list (val tree) 'right (left tree) (top tree))))))
  (define move-up
    (lambda (tree)
      (letrec [(top-value (val (top tree)))
               (sibling (caddr (top tree)))
               (ancestor (cadddr (top tree)))
               (this-child (list (val tree) (left tree) (right tree)))
               (children (if (equal? (cadr (top tree)) 'left)
                             (list this-child sibling)
                             (list sibling this-child)))]
        (list top-value (car children) (cadr children) ancestor))))
)
