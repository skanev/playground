; SICP exercise 2.77
;
; Louis Reasoner tries to evaluate the expression (magnitute z) where z is the
; object shown in Figure 2.24. To his surprise, instead of the answer 5 he
; gets an error message from apply-generic, saying there is no method for the
; operation magnitute on the types (complex). He shows this interaction to
; Alyssa P. Hacker, who says "The problem is that the complex-number selectors
; were never defined for complex numbers, just for polar and rectangular
; numbers. All you have to do to make this work is add the following to the
; complex package:"
;
; (put 'real-part '(complex) real-part)
; (put 'imag-part '(complex) imag-part)
; (put 'magnitute '(complex) magnitute)
; (put 'angle '(complex) angle)
;
; Describe in details why this works. As an example, trace through all the
; procedures called in evaluating the expression (magnitute z) where z is the
; object shown in Figure 2.24. In particular, how many times is apply-generic
; invoked? What procedure is dispatched in each case?

; It's straightforward.
;
; Whenever we call (magnitute z) we get to:
;
; (apply-generic 'magnitute z)
;
; where z is (complex (rectangular (3 . 4))). This, of course, fails since
; magnitute is not installed for complex. As soon as we install it, though, it
; would resolve to calling:
;
; (apply-generic 'magnitute '(rectangular (3 . 4)))
;
; That's because apply-generic strips the type tags whenever it passes the
; datum. This will dispatch to the code in the rectangular package, which
; would return 5.
;
; That said, apply-generic is called twice. The first time it is dispatched to
; back to magnitute, while the second - to the code in the rectangular
; package.
