; SICP exercise 1.24
;
; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime?
; (the Fermat method), and test each of the 12 primes you found in that
; exercise. Since the Fermat test has Θ(logn) growth, how would you expect the
; time to test primes near 1,000,000 to compare with the time needed to test
; primes near 1000? Do your data bear this out? Can you explain any discrepancy
; you find?

; Let's elaborate on the numbers we already have:
;
; +---------+----------------+----------------+----------------+
; | Number  | (+ divisor 1)  | (next divisor) | fermat test    |
; +---------+----------------+----------------+----------------+
; | 1009    | 0.001953125000 | 0.001953125000 | 0.010009765625 |
; | 1013    | 0.001953125000 | 0.001953125000 | 0.010009765625 |
; | 1019    | 0.002929687500 | 0.001953125000 | 0.010009765625 |
; | 10007   | 0.006835937500 | 0.004150390625 | 0.012207031250 |
; | 10009   | 0.007080078125 | 0.005126953125 | 0.011962890625 |
; | 10037   | 0.007080078125 | 0.004150390625 | 0.011962890625 |
; | 100003  | 0.018798828125 | 0.012939453125 | 0.014160156250 |
; | 100019  | 0.018066406250 | 0.011962890625 | 0.013916015625 |
; | 100043  | 0.019042968750 | 0.011962890625 | 0.013916015625 |
; | 1000003 | 0.055908203125 | 0.035888671875 | 0.015869140625 |
; | 1000033 | 0.055908203125 | 0.035888671875 | 0.015869140625 |
; | 1000037 | 0.055908203125 | 0.036132812500 | 0.015869140620 |
; +---------+----------------+----------------+----------------+
;
; I expect fast-prime? to be faster. The growth is as I expected. I am
; surprised that it is slower for smaller numbers.
;
; I have two problems with this exercise: (1) they complete too quickly on
; modern architectures and (2) (random) is limited to integers, which we
; quickly run out of before the execution times start getting interesting.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square n)
  (* n n))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))



(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
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
