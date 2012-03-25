; SICP exercise 2.15
;
; Eva Lu Ator, another user, has also noticed the different intervals computed
; by different but algebraically equivalent expressions. She says that a
; formula to compute with intervals using Alyssa's system will produce tighter
; error bounds if it can be written in such a form that no variable that
; represents an uncertain number is repeated. Thus, she says, par2 is a
; "better" program for parallel resistances than par1. Is she right? Why?

; She is definitelly right.
;
; It is easy to see from the results of exercise 2.14 that the more we perform
; operations with uncertain quantities, the bigger error we get. It is
; important to note, that this applies mainly to multiplication and division.
; 2A is exactly the same as AA.
