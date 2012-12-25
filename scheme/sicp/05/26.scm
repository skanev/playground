; SICP exercise 5.26
;
; Use the monitored stack to explore the tail-recursive property of the
; evaluator (section 5.4.2). Start the evaluator and define the iterative
; factorial procedure from section 1.2.1:
;
; (define (factorial n)
;   (define (iter product counter)
;     (if (> counter n)
;         product
;         (iter (* counter product)
;               (+ counter 1))))
;   (iter 1 1))
;
; Run the procedure with some small values of n. Record the maximum stack
; depth and the number of pushes required to compute n! for each of these
; values.
;
; a. You will find that the maximum depth required to evaluate n! is
; independent of n. What is the depth?
;
; b. Determine from your data a formula in terms of n for the total number of
; push operations used in evaluating n! for any n ≥ 1. Note that the number of
; operations used is a linear function of n and is thus determined by two
; constants.


; The results are:
;
; 1! takes (total-pushes = 64 maximum-depth = 10)
; 2! takes (total-pushes = 99 maximum-depth = 10)
; 3! takes (total-pushes = 134 maximum-depth = 10)
; 4! takes (total-pushes = 169 maximum-depth = 10)
; 5! takes (total-pushes = 204 maximum-depth = 10)
; 6! takes (total-pushes = 239 maximum-depth = 10)
; 7! takes (total-pushes = 274 maximum-depth = 10)
; 8! takes (total-pushes = 309 maximum-depth = 10)
; 9! takes (total-pushes = 344 maximum-depth = 10)
;
; The maximum depth is 10.
;
; The linear function is 35n + 29.
;
; The code that extracts it is below

(load-relative "tests/helpers/evaluator.scm")
(load-relative "tests/helpers/monitored-stack.scm")

(define code
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
         product
         (iter (* counter product)
               (+ counter 1))))
     (iter 1 1)))

(define machine (make-explicit-control-machine))

(set-register-contents! machine 'env the-global-environment)

(set-register-contents! machine 'exp code)
(start machine)

(for ([n (in-range 1 10)])
  (printf "~a! takes ~a\n" n (stack-stats-for machine (list 'factorial n))))
