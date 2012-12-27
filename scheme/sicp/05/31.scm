; SICP exercise 5.31
;
; In evaluating a procedure application, the explicit-control evaluator always
; saves and restores the env register around the evaluation of the operator,
; saves and restores env around the evaluation of each operand (except the
; final one), saves and restores argl around the evalutaion of each operand,
; and saves and restores proc around the evaluation of the operand sequence.
; For each of the following combinations, say which of these save and restore
; operations are superfluous and thus could be eliminated by the compiler's
; preserving mechanism.
;
; (f 'x 'y)
;
; ((f) 'x 'y)
;
; (f (g 'x) y)
;
; (f (g 'x) 'y)

; (f 'x 'y)
;
; There is no need to save and restore anything.
;
; ((f) 'x 'y)
;
; Again, there is no need to save/restore anything. Once the operator is
; evaluated, all the operands are fine, since they don't depend on the
; environment.
;
; (f (g 'x) y)
;
; * proc needs to be saved and restored around the evaluation of the first
;   operand
; * env needs to be saved and restored around the evaluation of the first
;   operand, if operands are evaluated left to right. Otherwise, if they are
;   evaluated right to left (as in the compiler), there is no need to save
;   env.
; * argl needs to be saved
; * All other are superfluous
;
; (f (g 'x) 'y)
;
; * proc needs to be saved and restored around the evaluation of the first
;   operand
; * argl needs to be saved
; * All other are superflous
