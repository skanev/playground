; SICP exercise 1.33
;
; You can obtain an even more general version of accumulate (exercise 1.32) by
; introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting filtered-accumulate abstraction takes the
; same arguments as accumulate, together with an additional predicate of one
; argument that specifies the filter. Write filtered-accumulate as a procedure.
; Show how to express the following using filtered-accumulate:
;
; a. the sum of the squares of the prime numbers in the interval a to b
; (assuming that you have a prime? predicate already written)
;
; b. the product of all the positive integers less than n that are relatively
; prime to n (i.e. all positive integer i < n such that GCD(i,n) = 1).

; I shall implemented filtered-accumulate both recursively and iteratively,
; just for the fun of it.

(define (filtered-accumulate combiner null-value term a next b use?)
  (define (iter a result)
    (cond ((> a b) result)
          ((use? a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (filtered-accumulate-rec combiner null-value term a next b use?)
  (cond ((> a b) null-value)
        ((use? a)
         (combiner (term a)
                   (filtered-accumulate-rec combiner null-value term (next a) next b use?)))
        (else
          (filtered-accumulate-rec combiner null-value term (next a) next b use?))))



(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a increment b prime?))

(define (square a)
  (* a a))

(define (increment n)
  (+ n 1))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))



(define (product-of-relative-primes-to n)
  (define (relatively-prime-to-n? x)
    (= (gcd n x) 1))
  (filtered-accumulate * 1 identity 1 increment n relatively-prime-to-n?))

(define (identity n)
  n)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
