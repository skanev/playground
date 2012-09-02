; SICP exercise 3.60
;
; With power series represented as streams of coefficients as in exercise
; 3.59, adding series is implemented by add-streams. Complete the definition
; of the following procedure for multiplying series:
;
;   (define (mul-series s1 s2)
;     (cons-stream <??> (add-streams <??> <??>)))
;
; You can test your procedure by verifying that sin²x + cos²x = 1, using the
; series from exercise 3.59

; It would have been good if I knew how to multiply series. Anyway, let's say
; we have (a₀ + A)(b₀ + B) where A and B are the remainder of the series.
; Then:
;
; (a₀ + A)(b₀ + B) = a₀b₀ + Ab₀ + Ba₀ + AB = a₀b₀ + a₀B + A(b₀ + B)
;
; In that expression, a₀b₀ is the first element of the series and the rest is
; the remaining elements. The solution is at the end.

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (neg-stream a) (stream-map - a))
(define (add-streams a b) (stream-map2 + a b))
(define (mul-streams a b) (stream-map2 * a b))
(define (div-streams a b) (stream-map2 / a b))
(define (scale-stream stream n) (stream-map (lambda (x) (* x n)) stream))

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

(define (integrate-series stream) (div-streams stream integers))
(define cosine-series (stream-cons 1 (integrate-series (neg-stream sine-series))))
(define sine-series (stream-cons 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
