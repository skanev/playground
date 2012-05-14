; SICP exercise 2.72
;
; Consider the encoding procedure that you designed in exercise 2.68. What is
; the order of growth in the number of steps needed to encode a symbol? Be
; sure to include the number of steps needed to search the symbol list at each
; node encountered. To answer this question in general is difficult. Consider
; the special case where the relative frequencies of the n symbols are as
; described in exercise 2.71, and give the order of growth (as a function of
; n) of the number of steps needed to encode the most frequent and the least
; frequent symbols in the alphabet.

; The most frequent symbol is obviously O(1). As for the least frequent, in
; the worst case, we need to search a list of size n on the first step, a list
; of size n - 1 on the second and so forth until only two leaf nodes remain.
; Thus, the complexity is 1 + 2 + 3 + … + n ≈ O(n²).
