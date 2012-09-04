; SICP exercise 3.72
;
; In a similar way to exercise 3.72 generate a stream of all numbers that can
; be written as the sum of two squares in three different ways (showing how
; they can be so written).

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

(define (three-ways-of-two-squares)
  (define (square x) (* x x))
  (define (weight pair) (+ (square (car pair)) (square (cadr pair))))
  (define (uniq stream)
    (if (= (stream-car stream) (stream-car (stream-cdr stream)))
        (uniq (stream-cons (stream-car stream) (stream-cdr (stream-cdr stream))))
        (stream-cons (stream-car stream) (uniq (stream-cdr stream)))))
  (define ordered-integers (weighted-pairs integers integers weight))
  (define (filter-numbers stream)
    (let ((p1 (stream-car stream))
          (p2 (stream-car (stream-cdr stream)))
          (p3 (stream-car (stream-cdr (stream-cdr stream)))))
      (if (= (weight p1) (weight p2) (weight p3))
          (stream-cons (weight p1)
                       (filter-numbers (stream-cdr stream)))
          (filter-numbers (stream-cdr stream)))))
  (uniq (filter-numbers ordered-integers)))
