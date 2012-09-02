; SICP exercise 3.53
;
; Without running the program, describe the elements of the stream defined by
;
;   (define s (cons-stream 1 (add-streams s s)))

; Every element is twice the previous, that is (1 2 4 8 16 32 ...), that is
; the powers of two.
