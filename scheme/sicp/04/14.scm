; SICP exercise 4.14
;
; Eva Lu Ator and Louis Reasoner are each experimenting with the meracircular
; evaluator. Eva types in the definition of map, and runs some test programs
; that use it. They work fine. Louis, in contrast, has installed the system
; version of map as a primitve for the metacircular evaluator. When he tries
; it, things go terribly wrong. Explain why Louis's map fails even though Eva's
; works.

; Scheme's map takes two arguments a procedure to apply and a list of things to
; apply it to. The procedure should, of course, be a Scheme procedure. When
; Louis installs the system version and calls map in the evaluator, the first
; argument to apply is a procedure in the evaluator, which is a list starting
; with either 'procedure, which is the evaluator's representation of a
; procedure. map then results with an error that it expected a procedure and
; it got a list.
