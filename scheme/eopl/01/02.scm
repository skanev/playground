; EOPL exercise 1.02
;
; What sets are defined by the following pairs of rules? Explain why.
;
;                     (n, k) ∈ S
; 1. (0, 1) ∈ S   ──────────────────
;                 (n + 1, k + 7) ∈ S
;
;                    (n, k) ∈ S
; 2. (0, 1) ∈ S   ───────────────
;                 (n + 1, 2k) ∈ S
;
;                        (n, i, j) ∈ S
; 3. (0, 0, 1) ∈ S   ─────────────────────
;                    (n + 1, j, i + j) ∈ S
;
;                          (n, i, j) ∈ S
; 4. (0, 1, 0) ∈ S   ─────────────────────────
;                    (n + 1, i + 2, i + j) ∈ S

; 1. This is the set of (n, 7n + 1) for n ∈ N. It is rather obvious why.
;
; 2. This is the set (n, 2ⁿ). You can tell that the first element grows in
;    increments of (like f(n) = n) and the second gets multiplied by two on
;    every iteration (like f(n) = 2ⁿ).
;
; 3. This set represents the Fibonacci numbers. If they are zero-indexed and
;    the first fibonaci number is 0, then this is (n, fib(n), fib(n + 1)).
;    This is easy to see, because the first element grows linearly,
;    independent from the other two. The second becomes the third from the
;    previous iteration and the third is the sum of the second and third from
;    the previous iteration, which is essentially the definition of Fibonacci.
;
; 4. This is an interesting one. It results to (n, 2n + 1, n²). It's easy to
;    see that the first one grows linearly, independent of the other two. The
;    second starts with 1 and grows by 2 on every iteration, thus 2n + 1. The
;    third one grows by 2n + 1 on every iteration, where n is the iteration
;    count. Thus, if the third element is calculated by g(n):
;
;    g(0) = 0         = n⁰
;    g(1) = 1 + 0 = 1 = n¹
;    g(2) = 3 + 1 = 4 = n²
;    ...
;    g(n + 1) = i + j = 2n + 1 + n² = n² + 2n + 1 = (n + 1)²
