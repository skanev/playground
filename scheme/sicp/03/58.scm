; SICP exercise 3.58
;
; Give an interpretation of the stream computed by the following procedure:
;
;   (define (expand num den radix)
;     (stream-cons
;       (quotient (* num radix) den)
;       (expand (remainder (* num radix) den) den radix)))
;
; (quotient is a primitive that returns the integer quotient of two integers.)
; What are the successive elements produced by (expand 1 7 10)? What is
; produced by (expand 3 8 10)?

; It produces the a stream of the digits of the decimal part of num/den in
; radix. (num * radix) / den is the first digit. Then it proceeds to calculate
; the first digit of the remainder of that division, which is the second digit
; of the decimal part of the number. An so on and so forth.
;
; (expand 1 7 10) produces (1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4)
; (expand 3 8 10) produces (3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
