; EOPL exercise 2.19
;
; A binary tree with empty leaves and with interior nodes labeled with
; integers could be represented using the grammar
;
;   Bintree ::= () | (Int Bintree Bintree)
;
; In this representation, implement the procedure number->bintree, which takes
; a number and produces a binary tree consisting of a single node, containing
; that number. Also implement current-element, move-to-left-son,
; move-to-right-son, at-leaf?, insert-to-left, and insert-to-right. For
; example,
;
; > (number->bintree 13)
; (13 () ())
; > (define t1 (insert-to-right 14
;                (insert-to-left 12)
;                  (number->bintree 13)))
; > t1
; (13
;   (12 () ())
;   (14 () ()))
; > (move-to-left t1)
; (12 () ())
; > (current-element (move-to-left-son t1))
; 12
; > (at-leaf? (move-to-right-son (move-to-left-son t1)))
; #t
; > (insert-to-left 15 t1)
; (13
;   (15
;     (12 () ())
;     ())
;   (14 () ()))

(define (number->bintree n)
  `(,n () ()))

(define (current-element tree)
  (car tree))

(define (move-to-left-son tree)
  (cadr tree))

(define (move-to-right-son tree)
  (caddr tree))

(define (at-leaf? tree)
  (null? tree))

(define (insert-to-left n tree)
  (list (car tree)
        (list n (cadr tree) '())
        (caddr tree)))

(define (insert-to-right n tree)
  (list (car tree)
        (cadr tree)
        (list n '() (caddr tree))))
