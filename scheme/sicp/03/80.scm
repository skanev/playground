; SICP exercise 3.80
;
; A series RLC circuit consists of a resistor, a capacitor, and an inductor
; connected in series, as shown in figure 3.36. If R, L, and C are the
; resistance, inductance and capacitance, then the relations between voltage
; (v) and current (i) for the three components are described by the equations
;
;   v(R) = i(R)R
;
;           di(L)
;   v(L) = L─────
;            dt
;
;           dv(C)
;   i(C) = C─────
;            dt
;
; and the circuit connections dictate the relations
;
;   i(R) = i(L) = -i(C)
;   v(C) = v(L) + v(R)
;
; Combining these equations shows that the state of the circuit (summarized by
; v(C), the voltage accross the capacitor, and i(L), the current in the
; inductor) is described by the pair of differential equations
;
;   dv(C)     i(L)
;   ───── = - ────
;    dt        C
;
;   di(L)   1       R
;   ───── = ─v(C) - ─i(L)
;    dt     L       L
;
; The signal-flow diagram representing this system of differential equations
; is shown in figure 3.37.
;
; Write a procedure RLC that takes as arguments the parameters R, L, and C of
; the circuit and the time increment dt. In a manner similar to that of the RC
; procedure of exercise 3.73, RLC should produce a procedure that takes the
; initial values of the state variables, v(C₀) and i(L₀), and produces a pair
; (using cons) of the streams of states v(C) and i(L). Using RLC, generate the
; pair of streams that models the behavior of a series RLC circuit with R = 1
; ohm, C = 0.2 farad, L = 1 henry, dt = 0.1 second, and initial values i(L₀) =
; 0 amps and v(C₀) = 10 volts.

; Wow. The electronics here are beyond me. To be honest - the math too.
; Anyway:

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

(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream (force delayed-integrand) dt)
                              int)))
  int)

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (- (/ 1 C))))
    (define dil (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (- (/ R L)))))
    (stream-map2 cons vc il)))

(define RLC1 (RLC 1 1 0.2 0.1))
