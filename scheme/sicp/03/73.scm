; SICP exercise 3.73
;
; We can model electrical circuits using streams to represent the values of
; currents or voltages at a sequence of times. For instance, suppose we have
; an RC circuit consisting of a resistor of resistance R and a capacitor C in
; series. The voltage response v of the circuit to an injected current i is
; determined by the formula in figure 3.33, whose structure is shown by the
; accomplanying signal-flow diagram:
;
;   [figure 3.33]
;
; Write a procedure RC that models this circuit. RC should take as inputs the
; values of R, C, and dt and should return a procedure that takes as inputs a
; stream representing the current i and an initial value for the capacitor
; voltage v₀ and produces as output the stream of voltages v. For example, you
; should be able to use RC to model an RC circuit with R = 5 ohms, C = 1
; farad, and 0.5-second time step by evaluating (define RC1 (RC 5 1 0.5)).
; This defines RC1 as a procedure that takes a stream representing the time
; sequence of currents and an initial capacitor voltage and produces the
; output stream of voltages.

; Man, this brings me a long time back. Also, I have no idea what I'm doing.

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (add-streams a b) (stream-map2 + a b))
(define (scale-stream stream n) (stream-map (lambda (x) (* n x)) stream))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))
(define ones-and-zeroes (stream-cons 1 (stream-cons 0 ones-and-zeroes)))

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

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt) int)))
  int)

(define (RC resistance capacity dt)
  (define (result stream initial-voltage)
    (add-streams (scale-stream stream resistance)
                 (integral (scale-stream stream (/ 1 capacity)) initial-voltage dt)))
  result)

(define RC1 (RC 5 1 0.5))
