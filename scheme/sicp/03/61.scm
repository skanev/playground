; SICP exercise 3.61
;
; Let S be a power series (exercise 3.59) whose constant term is 1. Suppose we
; want to find the power series 1/S, that is, the series X such that SX = 1.
; Write S = 1 + Sᵣ where Sᵣ is the part of S after the constant term. Then we
; can solve for X as follows:
;
;          S·X = 1
;   (1 + Sᵣ)·X = 1
;     X + Sᵣ·X = 1
;            X = 1 - Sᵣ·X
;
; In other words, X is the power series whose constant term is 1 and whose
; higher-order terms are given by the genative of Sᵣ times X. Use this idea to
; write a procedure invert-unit-series that computes 1/S for a power series S
; with constant term 1. You will need to use mul-series from exercise 3.60.

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

(define (invert-unit-series series)
  (stream-cons 1 (neg-stream (mul-series (stream-cdr series)
                                         (invert-unit-series series)))))
