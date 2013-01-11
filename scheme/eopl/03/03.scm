; EOPL exercise 3.03
;
; Why is subtraction a better choice than addition for our single arithmetic
; operation?

; Because we don't have negative numbers in our syntax. Thus, we cannot parse
; -x, but we can still represent it as -(0, x). This would not have been true
; if we used addition instead.
;
; Furthermore, we can represent x + y as -(x, -(0, y)) if we have subtraction.
; If we only have addition, there is on way to represent subtraction - (unless
; we had negation, but that is a second arithmetic operation).
