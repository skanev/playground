; SICP exercise 2.46
;
; A two-dimensional vector v running from the origin to a point can be
; represented as a pair consisting of an x-coordinate and a y-coordinate.
; Implement a data abstraction for vectors by giving a constructor make-vect
; and corresponding selectors xcor-vect and ycor-vect. In terms of your
; selectors and constructor, implement procedures add-vect, sub-vect, and
; scale-vect that perform the operations vector addition, vector subtraction,
; and multiplying a vector by a scalar.
;
; (x₁,y₁) + (x₂,y₂) = (x₁ + x₂,y₁ + y₂)
; (x₁,y₁) - (x₂,y₂) = (x₁ - x₂,y₁ - y₂)
;            s(x,y) = (sx,sy)

(define (make-vect x y)
  (list x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cadr vect))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
