; SICP exercise 2.26
;
; Suppose we define x and y to be two lists:
;
; (define x (list 1 2 3))
;
; (define y (list 4 5 6))
;
; What result is printed by the interpreter in response evaluating each of the
; following expressions:
;
; (append x y)
;
; (cons x y)
;
; (list x y)

; Another simple one:
;
; (append x y)
; (1 2 3 4 5 6)
;
; (cons x y)
; ((1 2 3) 4 5 6)
;
; (list x y)
; ((1 2 3) (4 5 6))
