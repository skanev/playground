; SICP exercise 3.71
;
; Numbers that can be expressed as the sum of two cubes in more than one way
; are sometimes called Ramanujan numbers, in honor of the mathematician
; Srinivasa Ramanujan. Ordered streams of pairs provide an elegant solution to
; the problem of computing these numbers. To find a number that can be written
; as the sum of two cubes in two different ways, we need only generate the
; stream of pairs of integers (i, j) weighted according to the sum i^3 + j^3
; (see exercise 3.70), then search the stream for two consecutive pairs with
; the same weight. Write a procedure to generate the Ramanujan numbers. The
; first such number is 1,729. What are the next five?

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (add-streams a b) (stream-map2 + a b))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))

(define (stream-take stream n)
  (if (= n 0)
      '()
      (cons (stream-car stream) (stream-take (stream-cdr stream) (- n 1)))))

(define (merge-weighted s t weight)
  (let* ((s0 (stream-car s))
         (t0 (stream-car t))
         (s0w (weight s0))
         (t0w (weight t0)))
    (if (< s0w t0w)
        (stream-cons s0 (merge-weighted (stream-cdr s) t weight))
        (stream-cons t0 (merge-weighted s (stream-cdr t) weight)))))

(define (weighted-pairs s t weight)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (ramanujan-numbers)
  (define (cube x) (* x x x))
  (define (weight pair) (+ (cube (car pair)) (cube (cadr pair))))
  (define ordered-integers (weighted-pairs integers integers weight))
  (define (filter-ramanujan stream)
    (let ((p1 (stream-car stream))
          (p2 (stream-car (stream-cdr stream))))
      (if (= (weight p1) (weight p2))
          (stream-cons (weight p1)
                       (filter-ramanujan (stream-cdr stream)))
          (filter-ramanujan (stream-cdr stream)))))
  (filter-ramanujan ordered-integers))
