; SICP exercise 3.65
;
; Use the series
;
;             1   1   1
;   ln2 = 1 - ─ + ─ - ─ + …
;             2   3   4
;
; to compute three sequences of approximations of the natural logarithm of 2,
; in the same way we did above for π. How rapidly do these sequences converge?

; The definitions are at the end of file. You can run it in order to see how
; many steps it takes to converge on a specific tolerance with all the
; sequences. This is the result from running it:
;
;   ln2-stream takes 9999 steps to tolerance 0.0001
;   ln2-stream-euler takes 12 steps to tolerance 0.0001
;   ln2-stream-accelarated takes 4 steps to tolerance 0.0001

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams a b) (stream-map2 + a b))
(define (neg-stream a) (stream-map - a))

(define (steps-to-tolerance stream tolerance)
  (define (count stream n)
    (let ((s1 (stream-car stream))
          (s2 (stream-car (stream-cdr stream))))
      (if (< (abs (- s1 s2))
             tolerance)
          n
          (count (stream-cdr stream) (+ n 1)))))
  (count stream 1))

(define (square x) (* x x))

(define (partial-sums stream)
  (define result
    (stream-cons (stream-car stream)
                 (add-streams (stream-cdr stream) result)))
  result)

(define (alternate-signs stream)
  (stream-cons (stream-car stream)
               (neg-stream (alternate-signs (stream-cdr stream)))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s))))

(define (accelarated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))



(define (ln2-summands n) (stream-cons (/ 1.0 n) (neg-stream (ln2-summands (+ n 1)))))
(define ln2-stream (partial-sums (ln2-summands 1)))
(define ln2-stream-euler (euler-transform ln2-stream))
(define ln2-stream-accelarated (accelarated-sequence euler-transform ln2-stream))

(define tolerance 0.0001)
(printf "ln2-stream takes ~s steps to tolerance ~s\n"
        (steps-to-tolerance ln2-stream tolerance)
        tolerance)
(printf "ln2-stream-euler takes ~s steps to tolerance ~s\n"
        (steps-to-tolerance ln2-stream-euler tolerance)
        tolerance)
(printf "ln2-stream-accelarated takes ~s steps to tolerance ~s\n"
        (steps-to-tolerance ln2-stream-accelarated tolerance)
        tolerance)
