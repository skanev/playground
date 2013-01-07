; EOPL exercise 2.20
;
; In the representation of binary trees in exercise 2.19 it is easy to move
; from a parent node to one of its sons, but it is impossible to move form a
; son to its parent without the help of context arguments. Extend the
; representation of lists in exercise 2.18 to represent nodes in a binary
; tree. As a hint, consider representing the portion of the tree above the
; current node by a reversed list, as in exercise 2.18.
;
; In this representation, implement the procedures from exercise 2.19. Also
; implement move-up, at-root? and at-leaf?.
;
(define (number->bintree n)
  `(,n () () ()))

(define (current-element tree)
  (car tree))

(define (move-to-left-son tree)
  (let ((son (cadr tree)))
    (if (null? son)
        (list '() '() '() tree)
        (list (car son) (cadr son) (caddr son) tree))))

(define (move-to-right-son tree)
  (let ((son (caddr tree)))
    (if (null? son)
        (list '() '() '() tree)
        (list (car son) (cadr son) (caddr son) tree))))

(define (move-up tree)
  (cadddr tree))

(define (at-leaf? tree)
  (null? (car tree)))

(define (at-root? tree)
  (null? (cadddr tree)))

(define (insert-to-left n tree)
  (list (car tree)
        (list n (cadr tree) '() '())
        (caddr tree)
        (cadddr tree)))

(define (insert-to-right n tree)
  (list (car tree)
        (cadr tree)
        (list n '() (caddr tree) '())
        (cadddr tree)))
