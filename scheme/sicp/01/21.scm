; SICP exercise 1.21
;
; Use the smallest-divisor procedure to find the smallest divisor of each of
; the following numbers: 199, 1999, 19999.

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

(printf "(smallest divisor of 199)   is ~a\n" (smallest-divisor 199))   ; 199
(printf "(smallest divisor of 1999   is ~a\n" (smallest-divisor 1999))  ; 1999
(printf "(smallest divisor of 19999) is ~a\n" (smallest-divisor 19999)) ; 7
