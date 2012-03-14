; SICP exercise 1.36
;
; Modify the fixed-point so that it prints the sequence of approximations it
; generates, using the newline and display primitives shown in exercise 1.22.
; Then find a solution to xˣ = 1000 by finding a fixed point of
; x ↦ log(1000)/log(x). (Use Scheme's primitive log procedure, which computes
; natural logarithms.) Compare the number of step this takes with and without
; average damping. (Note that you cannot start fixed-point with a guess of 1,
; as this ould cause divisino by log(1) = 0.)

; Curiously enough, the version with average damping takes significantly less
; steps. I expected it to be the other way around. I believe the reason is that
; without average damping, the guesses oscillate around the answer, while with
; it, the guesses approaches steadily from one direction.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iteration)
    (let ((next (f guess)))
      (display iteration)
      (display ". ")
      (display next)
      (newline)
      (if (close-enough? guess next)
        next
        (try next (+ iteration 1)))))
  (try first-guess 1))

(define (average x y)
  (/ (+ x y) 2))

(display "Without average damping:")
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

(newline)
(display "With average damping:")
(newline)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
