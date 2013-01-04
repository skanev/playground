; EOPL exercise 1.01
;
; Write inductive definitions of the following sets. Write each definition in
; all tree styles (top-down, bottom-up, and rules of inference). Using your
; rules, show the derivation of some sample elements of each set.
;
; 1. {3n + 2 | n ∈ N}
; 2. {2n + 3m + 1 | m, n ∈ N}
; 3. {(n, 2n + 1) | n ∈ N}
; 4. {(n, n²) | n ∈ N} Do not mention squaring in your rules. As a hint,
;    remember the equation (n + 1)² = n² + 2n + 1

; 1. {3n + 2 | m ∈ N}
;
;    top-down:
;
;      A natural number k is in S if and only if
;      1. k = 2, or
;      2. k - 3 ∈ S.
;
;    bottom-up:
;
;      Define the set S to be the smallest set contained in N and satisfying
;      the following two properties:
;      1. 2 ∈ S, and
;      3. if k ∈ S, then k + 3 ∈ S.
;
;    rules of inference:
;
;         ─────
;         2 ∈ S
;
;         k ∈ S
;      ───────────
;      (k + 3) ∈ S
;
;    sample derivation:
;
;      2 ∈ S
;      ─────
;      5 ∈ S
;      ─────
;      8 ∈ S
;
; 2. {2n + 3m + 1 | m, n ∈ N}
;
;    top-down:
;
;      A natural number k is in S if and only if
;      1. k = 1, or
;      2. k - 2 ∈ S, or
;      3. k - 3 ∈ S.
;
;    bottom-up:
;
;      Define the set S to be the smallest set contained in N and satisfying
;      the following three properties:
;      1. 1 ∈ S, or
;      2. if k ∈ S, then k + 2 ∈ S, or
;      3. if k ∈ S, then k + 3 ∈ S.
;
;    rules of inference:
;
;         ─────
;         1 ∈ S
;
;         k ∈ S
;      ───────────
;      (k + 2) ∈ S
;
;         k ∈ S
;      ───────────
;      (k + 3) ∈ S
;
;    sample derivation:
;
;       1 ∈ S
;       ─────
;       3 ∈ S
;       ─────
;       6 ∈ S
;       ─────
;       8 ∈ S
;      ──────
;      11 ∈ S
;
; 3. {(n, 2n + 1) | n ∈ N}
;
;    top-down:
;
;      A two-element list (a, b) is in S if and only if
;      1. a = 0, b = 1, or
;      2. (a - 1, b - 2) ∈ S.
;
;    bottom-up:
;
;      The set S is the smallest set of Scheme lists with two elements
;      satisfying the following two properties:
;      1. (0, 1) ∈ S
;      2. if (a, b) ∈ S, then (a + 1, b + 2) ∈ S
;
;    rules of inference:
;
;          ──────────
;          (0, 1) ∈ S
;
;          (a, b) ∈ S
;      ──────────────────
;      (a + 1, b + 2) ∈ S
;
;    sample derivation:
;
;      (0, 1) ∈ S
;      ──────────
;      (1, 3) ∈ S
;      ──────────
;      (2, 5) ∈ S
;      ──────────
;      (3, 8) ∈ S
;
; 4. {(n, n²) | n ∈ N}
;
;    top-down:
;
;      A two-element list (a, b) is in S if and only if
;      1. a = 0, b = 0, or
;      2. (a - 1, b - 2a - 1) ∈ S.
;
;    bottom-up:
;
;      The set S is the smallest set of Scheme lists with two elements
;      satisfying the following two properties:
;      1. (0, 1) ∈ S
;      2. if (a, b) ∈ S, then (a + 1, b + 2a + 1) ∈ S
;
;    rules of inference:
;
;             ──────────
;             (0, 0) ∈ S
;
;             (a, b) ∈ S
;      ───────────────────────
;      (a + 1, b + 2a + 1) ∈ S
;
;    sample derivation:
;
;      (0, 0) ∈ S
;      ──────────
;      (1, 1) ∈ S
;      ──────────
;      (2, 4) ∈ S
;      ──────────
;      (3, 9) ∈ S
