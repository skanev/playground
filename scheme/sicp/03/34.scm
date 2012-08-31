; SICP exercise 3.34
;
; Louis Reasoner wants to build a squarer, a constraint device with two
; terminals such that the value of connector b on the second terminal will
; always be the square of the value a on the first terminal. He proposes the
; following simple device made from a multiplier:
;
;   (define (squarer a b)
;     (multiplier a a b))
;
; There is a serious flaw in this idea. Explain.

; This will work alright when a is known and b is unknown. In the reverse
; case, however, we have a multiplier with a known product and two unknown
; multiplicands. There is no way to calculate what they are. Thus, it will
; simply not work when a is unknown.
