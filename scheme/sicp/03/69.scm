; SICP exercise 3.69
;
; Write a procedure triples that takes three infinite streams, S, T and U and
; produces the stream of triples (Sᵢ, Tᵣ, Uᵥ) such that i ≤ r ≤ v. Use triples
; to generate the stream of all Pythagorean triples of positive integers, i.e.
; the triples (i, j, k) such that i ≤ j and i² + j² = k².

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

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (stream-cons
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (append (list (stream-car s)) x))
                  (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define int-triples (triples integers integers integers))

(define (square x) (* x x))

(define (pythagorean? triple)
  (let ((a (car triple))
        (b (cadr triple))
        (c (caddr triple)))
    (= (+ (square a) (square b))
       (square c))))

(define pythagorean-triples (stream-filter pythagorean? int-triples))
