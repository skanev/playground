; EOPL exercise 1.14
;
; Given the assumption 0 ≤ n < length(v), prove that partial-vector-sum is
; correct.

; I don't grok this. It's extremely straightforward. Let's try using induction.
;
; 1. (partial-vector-sum v 0) returns the sum of indices in [0, 0]. This is
;    trivially obvious.
; 2. Let's assume that it is correct for k. For k + 1, partial-vector-sum
;    returns (vector-ref v k+1) + (partial-vector-sum v k). This is the sum of
;    this is the value of the partial sum from 0 to k + 1.
