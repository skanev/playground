; EOPL exercise 1.17
;
; (down lst) wraps parenthesis aroudn each top-level elements of lst.
;
; > (down '(1 2 3))
; ((1) (2) (3))
; > (down '((a) (fine) (idea)))
; (((a)) ((fine)) ((idea)))
; > (down '(a (more (complicated)) object))
; ((a) ((more (complicated))) (object))

(define down
  (lambda (lst)
    (map list lst)))
