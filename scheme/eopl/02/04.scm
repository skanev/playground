; EOPL exercise 2.04
;
; Consider the data type of stacks of values, with an interface consisting of
; the procedures empty-stack, push, pop, and empty-stack?. Write a
; specification for these operations in the style of the example above. Which
; operations are constructors and which are observers?

; (empty-stack)          = [∅]
; (push [stack] val)     = [val|stack]
; (pop [stack])          = { error if stack is ∅
;                          { [tail], if stack is [head|tail]
; (empty-stack? [stack]) = { #t if stack is ∅
;                          { #f otherwise
;
; empty-stack? is a obsever, all others are constructors.
;
; Note that this is really missing an observer top in order to be useful.
