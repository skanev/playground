; SICP exercise 1.22
;
; Most Lisp implementations include a primitve called runtime that returns an
; integer that specifies the amount of time the system has been running
; (measured, for example, in microseconds). The following timed-prime-test
; procedures, when called with an integer n, prints n and checks to see if n is
; prime. If n is prime, the procedure prints three asterisks followed by the
; amount of time used in performing the test.
;
; (define (timed-prime-test n)
;   (newline)
;   (display n)
;   (start-prime-test n (runtime)))
;
; (define (start-prime-test n start-time)
;   (if (prime? n)
;       (report-prime (- (runtime) start-time))))
;
; (define (report-prime elapsed-time)
;   (display " *** ")
;   (display elapsed-time)
;
; Using this procedure, write a procedure search-for-primes that checks the
; primality of consecutive odd integers in a specified range. Use your
; procedure to find the three smallest primes larger than 1000; larger than
; 10,000; larger than 100,000; larger than 1,000,000. Note the time neeed to
; test each prime. Since the testing algorithm has order of growth Θ(√n), you
; should expect that testing for primes around 10,000 should take about √10
; times as long as testing for primes around 1000. Do your timing data bear
; this out? How well do the data for testing for 100,000 and 1,000,000 support
; the √n prediction? Is your result compatible with the notion that programs on
; your machine run in time proportional to the number of steps required in the
; computation?

; Here are the numbers:
;
; 1009, 1013, 1019, 10007, 10009, 10037, 100003, 100019, 100043, 1000003,
; 1000033, 1000037
;
; Here is the time it takes to see if they are prime on my computer:
;
; 1009 *** 0.001953125
; 1013 *** 0.001953125
; 1019 *** 0.0029296875
; 10007 *** 0.0068359375
; 10009 *** 0.007080078125
; 10037 *** 0.007080078125
; 100003 *** 0.018798828125
; 100019 *** 0.01806640625
; 100043 *** 0.01904296875
; 1000003 *** 0.055908203125
; 1000033 *** 0.055908203125
; 1000037 *** 0.055908203125
;
; Even with numbers as 1,000,000, it is easily illustrated that the √n
; prediction is true. Programs do run in time proportional to the number of
; steps required in the computation, at least on this machine. Just run the
; program to verify.
;
; I wonder if some day I will run this on a machine, that will show different
; results.

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



(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square a)
  (* a a))

(define (divides? a b)
  (= (remainder b a) 0))



(define (search-for-primes lower-limit)
  (newline)
  (define (iter n number)
    (cond ((= n 0) 0)
          ((prime? number)
            (display number)
            (newline)
            (iter (- n 1) (+ number 1)))
          (else (iter n (+ number 1)))))

  (iter 3 lower-limit))



(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)

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
