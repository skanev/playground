; SICP exercise 2.53
;
; What would be the interpreter print in response to evaluating each of the
; following expressions?
;
; (list 'a 'b 'c)
; (list (list 'george))
; (cdr '((x1 x2) (y1 y2)))
; (cadr '((x1 x2) (y1 y2)))
; (pair? (car '(a short list)))
; (memq 'red '((red shoes) (blue socks)))
; (memq 'red '(red shoes blue coks))

; (list 'a 'b 'c)
; '(a b c)
;
; (list (list 'george))
; '((george))
;
; (cdr '((x1 x2) (y1 y2)))
; '((y1 y2))
;
; (cadr '((x1 x2) (y1 y2)))
; '(y1 y2)
;
; (pair? (car '(a short list)))
; #f
;
; (memq 'red '((red shoes) (blue socks)))
; #f
;
; (memq 'red '(red shoes blue socks))
; '(red shoes blue socks)
