; EOPL exercise 3.02
;
; Give an expressed value val ∈ ExpVal for which ⎡ ⎣ val⎦ ⎤ ≠ val.

; This is tricky. We don't have it in the LET language. let's explore a
; language has rational numbers in ExpVal, but the implementation language
; does not have that notion. Instad, expval->num converts the rational number
; to a float. In that language
;
; (num-val (expval->num ⎡ 1/3⎤))
;
; would not return one third, but a rational number that is close to it.
