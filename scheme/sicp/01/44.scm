; SICP exercise 1.44
;
; The idea of smoothing a function is an important concept in signal
; processing.  If f is a function and dx is some small number, then the
; smoothed version of f is the function whose value at a point x is the average
; f(x - dx), f(x) and f(x + dx). Write a procedure smooth that takes as input a
; procedure that computes f and returns a procedure that computes the smoothed
; f. It is sometimes valuable to repeatedly smooth a function (that is, smooth
; the smoothed function, and so on) to obtain the n-fold smoothed function.
; Show how to generate the n-fold smoothed function of any given function using
; smooth and repeated from exercise 1.43.

(define (smoothed f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smoothed f n)
  ((repeated smoothed n) f))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define dx 0.0000001)
