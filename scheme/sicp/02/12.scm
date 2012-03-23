; SICP exercise 2.12
;
; Define a constructor make-center-percent that takes a center and a percentage
; tolerance and produces the desired interval. You must also define a selector
; percent that produces the percentage tolerance for a given interval. The
; center selector is the same as the one shown above.

(define (make-center-percent value tolerance)
  (let ((width (* value (/ tolerance 100))))
    (make-interval (- value tolerance) (+ value tolerance))))

(define (percent i)
  (* (/ (width i) (center i))
     100))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-interval x y)
  (cons x y))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))
