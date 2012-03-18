; SICP exercise 1.45
;
; We saw in section 1.3.3 that attempting to compute square roots by naively
; finding a fixed point of x ↦ x/y does not converge, and that this can be
; fixed by average damping. The same method works for finding cube roots as
; fixed points of the average-damped y ↦ x/y². Unfortunatelly, the process does
; not work for fourth roots — a single average damp is not enough to make a
; fixed-point search for y ↦ x/y³ converge. On the other hand, if we average
; damp twice (i.e., use the average damp of the average damp of y ↦ x/y³) the
; fixed-point search does converge. Do some experiments to determine how many
; average damps are required to compute nth roots as a fixed-point search based
; upon repeated average damping of y ↦ x/yⁿ⁻¹. Use this to implement a simple
; procedure for computing nth roots using fixed-point, average-damp, and the
; repeated procedure of exercise 1.43. Assume that any arithmetic operations
; you need are available as primitives.

; After a few experiments, I found out that we need to do log₂n avergage damps
; to approximate the nth root. So, here's the function

(define (nth-root n x)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (ceiling (/ (log n) (log 2))))
                            1.0))

; And here is all the code we need to get it to run

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x)
    (/ (+ (f x) x)
       2)))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define tolerance 0.000001)
