; SICP exercise 1.25
;
; Allysa P. Hacker complains that we went to a lot of extra work in writing
; expmod. After all, she says, since we already know how to compute exponents,
; we could have simply written
;
; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))
;
; Is she correct? Would this procedure serve as well for our fast prime tester?
; Explain.

; She is correct. We can define expmod that way and it would certainly return
; correct results. However, it will not deserve being called "fast-prime?",
; since it quickly becomes dramatically slower. Here is a comparison:
;
; +----------------+--------------------+
; | exercise 24    | exercise 25        |
; +----------------+--------------------+
; | 0.101074218750 |     0.751953125000 |
; | 0.004150390625 |     0.282958984375 |
; | 0.002929687500 |     0.229980468750 |
; | 0.003173828125 |     7.984863281250 |
; | 0.004150390625 |    10.800781250000 |
; | 0.003173828125 |     5.987060546875 |
; | 0.077880859375 |   419.326904296875 |
; | 0.024169921875 |   460.801025390625 |
; | 0.022949218750 |   401.984130859375 |
; | 0.028076171875 | 12811.398193359375 |
; | 0.020019531250 | 15219.705078125000 |
; | 0.028076171870 | 18424.709228515620 |
; +----------------+--------------------+
;
; This happens, because we end up calculating exponents with very large numbers
; Multiplication and division is particularly slow with those. Allysa's expmod
; quickly starts multiplying them, while the one we wrote goes into great
; lengths of avoiding it.

(define (fast-expt base power)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (* b b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))

  (iter 1 base power))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

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
