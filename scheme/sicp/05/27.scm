; SICP exercise 5.27
;
; For comparison with exercise 5.26, explore the behavior of the following
; procedure for computing factorials recursively:
;
; (define (factorial n)
;   (if (= n 1)
;       1
;       (* (factorial (- n 1)) n)))
;
; By running this procedure with the monitored stack, determine, as a function
; of n, the maximum depth of the stack and the total number of pushes used in
; evaluating n! for n ≥ 1. (Again, these functions will be linear). Summarize
; your experiments by filling in the following table with the appropriate
; expressions in terms of n:
;
; <table>
;
; The maximum depth is a measure of the amount of space used by the evaluator
; in carrying out the computation, and the number of pushes correlates well
; with the time required.

; The results are:
;
; 1! takes (total-pushes = 16 maximum-depth = 8)
; 2! takes (total-pushes = 48 maximum-depth = 13)
; 3! takes (total-pushes = 80 maximum-depth = 18)
; 4! takes (total-pushes = 112 maximum-depth = 23)
; 5! takes (total-pushes = 144 maximum-depth = 28)
; 6! takes (total-pushes = 176 maximum-depth = 33)
; 7! takes (total-pushes = 208 maximum-depth = 38)
; 8! takes (total-pushes = 240 maximum-depth = 43)
; 9! takes (total-pushes = 272 maximum-depth = 48)
;
; This implies that the maximum depth is 5n + 3 and the total pushes are
; 32n - 16.
;
;                     | Maximum depth | Number of pushes
; --------------------+---------------+-----------------
; Recursive factorial |        5n + 3 |         32n - 16
; --------------------+---------------+-----------------
; Iterative factorial |            10 |         35n + 29
; --------------------+---------------+-----------------

(load-relative "tests/helpers/evaluator.scm")
(load-relative "tests/helpers/monitored-stack.scm")

(define code
  '(define (factorial n)
     (if (= n 1)
       1
       (* (factorial (- n 1)) n))))

(define machine (make-explicit-control-machine))

(set-register-contents! machine 'env the-global-environment)

(set-register-contents! machine 'exp code)
(start machine)

(for ([n (in-range 1 10)])
  (printf "~a! takes ~a\n" n (stack-stats-for machine (list 'factorial n))))
