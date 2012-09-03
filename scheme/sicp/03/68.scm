; SICP exercise 3.68
;
; Louis Reasoner thinks that building a stream of pairs from three pars in
; unnecessarily complicated. Instead of separating the pair (S₀, T₀) from the
; rest of the pairs in the first row, he proposes to work with the whole first
; row, as follows:
;
;   (define (pairs s t)
;     (interleave (stream-map (lambda (x) (list (stream-car s) x))
;                             t)
;                 (pairs (stream-cdr s) (stream-cdr t))))
;
; Does this work? Consider what happens if we evaluate (pairs integers
; integers) using Louis's definition of pairs.

; Although conceptually sound, this doesn't work in practice. When pairs is
; called, it has to execute interleave. Before doing that, it needs to
; evaluate the arguments, the second of which is a call to pairs. Since call
; is not delayed, we end up in an recursion, where pairs keeps calling itself
; with the cdr's of its arguments. Since both streams are infinite, this is
; never bound to end.
