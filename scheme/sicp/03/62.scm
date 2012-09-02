; SICP exercise 3.62
;
; Use the result of exercise 3.60 and exercise 3.61 to define a procedure
; div-series that divides two power series. div-series should work for any two
; series, provided that the denominator series begins with a nonzero constant
; term. (If the denominator has a zero constant term, the div-series should
; signal an error.) Show how to use div-series together with the result of
; exercise 3.59 to generate the power series for tangent.

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

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series series)
  (stream-cons 1 (neg-stream (mul-series (stream-cdr series)
                                         (invert-unit-series series)))))

(define (div-series a b)
  (if (= (stream-car b) 0)
      (error "Cannot divide by a power series with constant term = 0")
      (mul-series a (invert-unit-series b))))

(define cosine-series (stream-cons 1 (integrate-series (neg-stream sine-series))))
(define sine-series (stream-cons 0 (integrate-series cosine-series)))
(define tangent-series (div-series sine-series cosine-series))
