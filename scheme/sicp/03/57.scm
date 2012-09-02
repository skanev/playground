; SICP exercise 3.57
;
; How many additions are performed when we compute the nth Fibonacci number
; using the definition of fibs based on the add-streams procdure? Show that
; the number of additions would be exponentially greater if we had implemented
; (delay <exp>) simply as (lambda () <exp>), without using the optimization
; provided by the memo-proc procedure described in section 3.5.1

; There are n - 1 additions performed for computing the nth fibonacci number.
; In order to calculate the kth number, we're adding the (k - 1)th and the
; (k - 2)th.
;
; If we don't memoize, we'll have to compute the two previous numbers for each
; number we want to calculate. Recursively. This is the definition of
; exponential.
