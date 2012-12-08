; SICP exercise 4.19
;
; Ben Bitdiddle, Alyssa P. Hacker, and Eva Lu Ator are arguing about the
; desired result of evaluating the expression
;
;   (let ((a 1))
;     (define (f x)
;       (define b (+ a x))
;       (define a 5)
;       (+ a b))
;     (f 10))
;
; Ben asserts that the result should be obtained using the sequential rule for
; define: b is defined to be 11, then a is defined to be 5, so the result is
; 16. Alyssa objects that mutual recursion requires the simultaneous scope
; rule for internal procedure definitions, and that is unreasonable to treat
; procedure names differently from other names. Thus, she argues, for the
; mechanism implemented in exercise 4.16. This would lead to a being
; unassigned at the time that the value for b is to be computed. Hence, in
; Alyssa's view, the procedure should produce an error. Eva has a third
; opinion. She says that if the definitions of a and b are truly meant to be
; simultaneous, then the value 5 for a should be used in evaluating b. Hence,
; in Eva's view a should be 5, b should be 15, and the result should be 20.
; Which (if any) of these viewpoints do you support? Can you devise a way to
; implement internal definitions so that they behave as Eva prefers?

; Personally, I dislike the distinction between define and set!. I prefer both
; behaving the same way, that is, allowing assignment to an unassigned name
; and setting to an assigned one. This removes the distinction between
; definition and assignment, which I find superflous at best. Granted, it
; imposes an order of evaluating, but that is simple enough to understand. The
; downside is that such a strategy would not allow name shadowing with define.
; This is not a problem with variable names (since you can use let), but
; results in an awkward syntax for shadowing functions.
;
; If you have to live with having define able to shadow names, I would prefer
; Alyssa's approach, because of its simplicity.
;
; Finally, Eva's preference would require finding the dependencies between
; definitions and reordering them. My guy feeling is that this can be done
; compile time in an efficient manner, but it is more complex than just
; sticking with Alyssa's approach. It also introduces a harder to understand
; system, where the user needs to be aware of this reordering in order to
; understand why their programming is behaving the way they observe.
;
; I find that simple-to-understand interpreter outweights fancy, but rarely
; used features.
