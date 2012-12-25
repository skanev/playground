; SICP exercise 5.29
;
; Monitor the stack operations in the tree-recursive Fibonacci computation:
;
; (define (fib n)
;   (if (< n 2)
;       n
;       (+ (fib (- n 1)) (fib (- n 2)))))
;
; a. Give a formula in terms of n for the maximum depth of the stack required
; to compute Fib(n) for n ≥ 2. Hint: In section 1.2.2 we argued that the space
; used by this process grows linearly with n.
;
; b. Give a formula for the total number of pushes used to compute Fib(n) for
; n ≥ 2. You should find that the number of pushes (which correlates well with
; the time used) grows exponentially with n. Hint: Let S(n) be the number of
; pushes used in computing Fib(n). You should be able to argue that there is a
; formulate that expresses S(n) in terms of S(n - 1), S(n - 2), and some fixed
; "overhead" constant k that is independent of n. Give the formula, and say
; what k is. Then show that S(n) can be expressed as a.Fib(n + 1) + b and give
; the values of a and b.

; The results are:
;
; fib(0) takes (total-pushes = 16 maximum-depth = 8)
; fib(1) takes (total-pushes = 16 maximum-depth = 8)
; fib(2) takes (total-pushes = 72 maximum-depth = 13)
; fib(3) takes (total-pushes = 128 maximum-depth = 18)
; fib(4) takes (total-pushes = 240 maximum-depth = 23)
; fib(5) takes (total-pushes = 408 maximum-depth = 28)
; fib(6) takes (total-pushes = 688 maximum-depth = 33)
; fib(7) takes (total-pushes = 1136 maximum-depth = 38)
; fib(8) takes (total-pushes = 1864 maximum-depth = 43)
; fib(9) takes (total-pushes = 3040 maximum-depth = 48)
;
; a. The maximum stack depth for any n is
;
;   5n + 3
;
; b. The number of pushes is
;
;   S(n) = S(n - 1) + S(n - 2) + 40
;
; or
;
;   S(n) = 56.Fib(n + 1) - 40

(load-relative "tests/helpers/evaluator.scm")
(load-relative "tests/helpers/monitored-stack.scm")

(define code
  '(define (fib n)
     (if (< n 2)
       n
       (+ (fib (- n 1)) (fib (- n 2))))))

(define machine (make-explicit-control-machine))

(set-register-contents! machine 'env the-global-environment)

(set-register-contents! machine 'exp code)
(start machine)

(for ([n (in-range 0 10)])
  (printf "fib(~a) takes ~a\n" n (stack-stats-for machine (list 'fib n))))
