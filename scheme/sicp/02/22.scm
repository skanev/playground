; SICP exercise 2.22
;
; Louis Reasoner tries to rewrite the first square-list procedure on Exercise 2.21
; so that it evolves an iterative process:
;
; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons (square (car things))
;                     answer))))
;   (iter items nil))
;
; Unfortunatelly, defining square-list this way produces the answer list in the
; reverse order of the one desired. Why?
;
; Louis then tries to fix his bug by interchanging the arguments to cons:
;
; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons answer
;                     (square (car things))))))
;   (iter items nil))
;
; This doesn't work either. Explain.

; In the first program, we are accumulating the answer by adding the square of
; each item to the front of the list. It is easy to see that (car items)
; becomes the last element of answer, (cadr items) becomes the second to last
; and so on.
;
; As for the second version, the result is not a list. It is a pair, where the
; cdr in the squared item and the car is a pair with the next square (stored in
; the cadr of result). Furthermore, it is still in the wrong order.
