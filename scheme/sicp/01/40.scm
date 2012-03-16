; SICP exercise 1.40
;
; Define a procedure cubic that can be used together with newtons-method
; procedure in expressions of the form
;
; (newtons-method (cubic a b c) 1)
;
; to approximate zeroes of the cubic x³ + ax² + bx + c

; It is simpler than it sounds - we just define a lambda, that computes
; x³ + ax² + bx + c

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define dx 0.00001)
(define tolerance 0.00001)
