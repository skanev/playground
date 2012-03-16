; SICP exercise 1.28
;
; One variant of the Fermat test that cannot be fooled is called the
; Miller-Rabin test. This starts from an alternate form of Fermat's Little
; Theorem, which states that if n is a prime number and a is any positive
; integer less than n, then a raised to the (n - 1)-st power is congruent to 1
; modulo n. To test the primality of a number n by the Miller-Rabin test, we
; pick a random number a < n and raise a to the (n - 1)-st power module n using
; the expmod procedure. However, whenever we perform the squaring step in
; expmod, we check to see if we have discovered a "nontrivial square root of 1
; modulo n," that is, a number not equal to 1 or n - 1 whose square is equal to
; 1 modulo n. It is possible to prove that if such a nontrivial square root of
; 1 exists, then n is not prime. It is also possible to prove that if n is an
; odd number that is not prime, then, for at least half of the numbers a < n,
; computing aⁿ⁻¹ in this way will reveal a nontrivial square root of 1 modulo
; n. (This is why the Miller-Rabin test cannot be fooled.) Modify the expmod
; procedure to signal if it discovers a nontrivial square root of 1, and use
; this to implement the Miller-Rabin test with a procedure analogous to
; fermat-test. Check your procedure by testing various known primes and
; non-primes. Hint: One convenient way to make expmod singal is to have it
; return 0.

; Changing the algorithm produces produces results I did not expect.
;
; Let's take a look at the amount of numbers a, such that a < n, for which aⁿ⁻¹
; is congruent to 1 modulo n. If n is prime, this is true for all numbers
; 1 < a < n. I expected that to be true for Carmichael numbers, but I turned
; out to be wrong - it's not all numbers, but a fairly large percent of them.
; For example, if n = 66011, for approximatelly 80% of the numbers a < n, aⁿ⁻¹
; is congruent to 1 modulo n. The large the percentage, the more likely the
; test will be fooled.
;
; However, if we introduce the check for non-trivial square root of 1 modulo n,
; the amount of numbers that fool the test drops dramatically. Here's a table
; for the Carmichael numbers in said footnote:
;
; +------+-----------+----------+
; | n    | w/o sqrt1 | w/ sqrt1 |
; +------+-----------+----------+
; | 561  |       57% |       2% |
; | 1105 |       70% |       3% |
; | 1729 |       75% |       9% |
; | 2465 |       73% |       3% |
; | 2821 |       77% |      10% |
; | 6601 |       80% |       5% |
; +------+-----------+----------+
;
; Anyway, here's the code. I cannot shake the feeling that I am getting
; something wrong.

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (miller-rabin-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square 
                      (zero-if-non-trivial-sqrt 
                        (miller-rabin-expmod base (/ exp 2) m)
                        m))
                    m))
        (else
         (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                    m))))

(define (square n)
  (* n n))

(define (zero-if-non-trivial-sqrt x n)
  (if (and (not (= x 1))
           (not (= x (- n 1)))
           (= (remainder (square x) n) 1))
      0
      x))
