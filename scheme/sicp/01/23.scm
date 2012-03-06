; SICP exercise 1.23
;
; The smallest-divisor procedure shown at the start of this section does lots
; of needless testing: After it checks to see if the number is divisible by 2
; there is no pint in checking to see if it is divisible by any larger even
; numbers.  This suggests that the values used for test-divisor should not be
; 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, .... To implement this change,
; define a procedure next that returns 3 if its input is equal to 2 and
; otherwise returns its input plus 2. Modify the smallest-divisor procedure to
; use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test
; incorporating this modified version of smallest-divisor, run the test for
; each of the 11 primes found in exercise 1.22. Since this modification halves
; the number of test steps, you should expect it to run about twice as fast. Is
; this expectation confirmed? If not, what is the observed ratio of the speeds
; of the two algorithms and how do you explain the fact that it is different
; from 2?

; Let's compare the numbers:
;
; +---------+----------------+----------------+
; | Number  | (+ divisor 1)  | (next divisor) |
; +---------+----------------+----------------+
; | 1009    | 0.001953125000 | 0.001953125000 |
; | 1013    | 0.001953125000 | 0.001953125000 |
; | 1019    | 0.002929687500 | 0.001953125000 |
; | 10007   | 0.006835937500 | 0.004150390625 |
; | 10009   | 0.007080078125 | 0.005126953125 |
; | 10037   | 0.007080078125 | 0.004150390625 |
; | 100003  | 0.018798828125 | 0.012939453125 |
; | 100019  | 0.018066406250 | 0.011962890625 |
; | 100043  | 0.019042968750 | 0.011962890625 |
; | 1000003 | 0.055908203125 | 0.035888671875 |
; | 1000033 | 0.055908203125 | 0.035888671875 |
; | 1000037 | 0.055908203125 | 0.036132812500 |
; +---------+----------------+----------------+
;
; The ratio is between 3/2 and 2. We can approximate it to 3/2. We get less
; than 2, because even if we halve the steps, we add an additional overhead for
; each step - testing whether the number is 2.

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (square a)
  (* a a))

(define (divides? a b)
  (= (remainder b a) 0))



(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (void)))

(define (runtime)
  (current-inexact-milliseconds))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))



(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)

(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
