; SICP exercise 3.70
;
; It would be nice to be able to generate streams in which the pairs appear in
; some useful order, rather than in the order that results from an ad hoc
; interleaving process. We can use a technique similar to the merge procedure
; in figure 3.56, if we define a way to say that one pair of integers is "less
; than" another. One way to do this is to define a "weighting function"
; W(i, j) and stipulate that (i₁, j₁) is less than (i₂, j₂) if
; W(i₁, j₁) < W(i₂, j₂). Write a procedure merge-weighted that is like merge,
; except that merge-weighted takes an additional argument weight, which is a
; procedure that computes the weight of a pair, and is used to determine the
; order in which elements should appear in the resulting merged stream. Using
; this, generalize pairs to a procedure weighted-pairs that takes two streams,
; together with a procedure that computes a weighting function, and generates
; the stream of pairs, ordered according to weight. Use your procedure to
; generate
;
; a. the stream of all pairs of positive integers (i, j) with i ≤ j ordered
; according to the sum i + j
;
; b. the stream of all pairs of positive integers (i, j) with i ≤ j, where
; neither i nor j is divisable by 2, 3, or 5 and the pairs are ordered
; according to the sum 2i + 3j + 5ij.

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

(define (a-pairs)
  (define (pair-sum pair) (+ (car pair) (cadr pair)))
  (weighted-pairs integers integers pair-sum))

(define (b-pairs)
  (define (useful? integer)
    (not (or (= (remainder integer 2) 0)
             (= (remainder integer 3) 0)
             (= (remainder integer 5) 0))))
  (define (weight pair)
    (let ((i (car pair)) (j (cadr pair)))
      (+ (* 2 i)
         (* 3 j)
         (* 5 i j))))
  (define useful-integers (stream-filter useful? integers))
  (weighted-pairs useful-integers useful-integers weight))
