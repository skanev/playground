; SICP exercise 2.03
;
; Implement a representation for rectangles in a plane. (Hint: You may want to
; make use of exercise 2.2.) In terms of your constructors and selectors,
; create procedures that compute the perimeter and the area of a given
; rectangle. Now implement a different representation of rectangles. Can you
; design your system with suitable abstraction barriers, so that the same
; perimeter and area procedures will work using either representation?

; Of course I can! Here it is:

(define (perimeter rectangle)
  (let ((top-left (top-left-rectangle rectangle))
        (bottom-right (bottom-right-rectangle rectangle)))
    (* (+ (abs (- (x-point bottom-right) (x-point top-left)))
          (abs (- (y-point bottom-right) (y-point top-left))))
       2)))

(define (area rectangle)
  (let ((top-left (top-left-rectangle rectangle))
        (bottom-right (bottom-right-rectangle rectangle)))
    (abs (* (- (x-point bottom-right) (x-point top-left))
            (- (y-point bottom-right) (y-point top-left))))))

; This is the first representation of rectangles I used:

(define (make-rectangle top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left-rectangle rectangle)
  (car rectangle))

(define (bottom-right-rectangle rectangle)
  (cdr rectangle))

; This is the second:

(define (make-rectangle top-left bottom-right)
  (let ((width (abs (- (x-point bottom-right) (x-point top-left))))
        (height (abs (- (y-point bottom-right) (y-point top-left)))))
    (cons top-left (cons width height))))

(define (top-left-rectangle rectangle)
  (car rectangle))

(define (bottom-right-rectangle rectangle)
  (let ((top-left (car rectangle))
        (width (car (cdr rectangle)))
        (height (cdr (cdr rectangle))))
  (make-point (+ (x-point top-left) width)
              (+ (y-point top-left) height))))

; And this is the point:

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))
