; SICP exercise 3.82
;
; Redo exercise 3.5 on Monte Carlo integration in terms of streams. The stream
; version of estimate-integral will not have an argument telling how many
; trials to perform. Instead, it will produce a stream of estimates based on
; succesively more trials.

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-take stream n)
  (if (= n 0)
      '()
      (cons (stream-car stream) (stream-take (stream-cdr stream) (- n 1)))))

(define random-modulus 4294967296)
(define random-init (modulo (current-milliseconds) random-modulus))

(define (rand-update number)
  (let ((modulus random-modulus)
        (multiplier 1664525)
        (increment 1013904223))
    (modulo (+ (* multiplier number) increment) modulus)))

(define random-integers
  (stream-cons random-init
               (stream-map rand-update random-integers)))

(define random-floats
  (stream-map (lambda (x) (exact->inexact (/ x random-modulus))) random-integers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
      (/ passed (+ passed failed))
      (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (map-successive-pairs f s)
  (stream-cons
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (estimate-integral predicate x1 x2 y1 y2)
  (define (scale-within value low high)
    (+ (* value (- high low)) low))
  (define experiment-stream
    (map-successive-pairs
      (lambda (a b)
        (predicate (scale-within a x1 x2)
                   (scale-within b y1 y2)))
      random-floats))
  (monte-carlo experiment-stream 0 0))

(define (estimate-pi tries)
  (define (circle x y)
    (<= (+ (* x x) (* y y)) 1))
  (* (stream-ref (estimate-integral circle -1 1 -1 1) tries)
     4.0))
