; SICP exercise 1.19
;
; There is a clever algorithm for computing the Fibonacci numbers in a
; logarithmic number of steps. Recall the transformation of the state variables
; a and b in the fib-iter process of section 1.2.2: a ← a + b and b ← a. Call
; this transformation T, and observe that applying T over and over again n
; times, starting with 1 and 0, produces the pair Fib(n+1) and Fib(n). In other
; words, the Fibonacci numbers are produced by applying Tⁿ, the nth power of
; the transformation T, starting with the pair (1, 0). Now consider T to be the
; special case of p = 0 and q = 1 in a family of transformations T(p,q), where
; T(p,q) transforms the pair (a,b) according to a ← bq + aq + ap and
; b ← bp + aq. Show that if we apply such a transformation T(p,q) twice, the
; effect is the same as using a single transformation T(p',q') of the same
; form, and compute p' and q' in terms of p and q.
;
; This gives us an explicit way to square these transformations, and thus we
; can compute Tⁿ using successive squaring, as in the fast-expt procedure. Put
; this all together to complete the following procedure, which runs in a
; logarithmic number of steps.
;
; (define (fib n)
;   (fib-iter 1 0 0 1 n))
;
; (define (fib-iter a b p q count)
;   (cond ((= count 0) b)
;         ((even? count) (fib-iter a
;                                  b
;                                  <??>   ; compute p'
;                                  <??>   ; compute q'
;                                  (/ count 2)))
;         (else (fib-iter (+ (* b q) (* a q) (* a p))
;                         (+ (* b p) (* a q))
;                         p
;                         q
;                         (- count 1)))))

; Here are some inferences:
;
; After T(p,q)
;
; a = bq + aq + ap
; b = bp + aq
;
; If we apply T(p,q) again, we get:
;
; a = bpq + aqq + bqq + aqq + apq + bpq + apq + app
; b = bpp + apq + bqq + aqq + apq
;
; If we normalize it:
;
; a = 2bpq + bq² + 2aq² + 2apq + ap²
; b = bp² + bq² + 2apq + aq²
;
; If we group it:
;
; a = b(q² + 2pq) + a(q² + 2pq) + a(p² + q²)
; b = b(p² + q²) + a(q² + 2pq)
;
; Clearly:
;
; p' = p² + q²
; q' = q² + 2pq

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (* p p) (* q q))
                                 (+ (* q q) (* 2 p q))
                                 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
