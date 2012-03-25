; SICP exercise 2.14
;
; Demonstrate that Lem is right. Investigate the behavior of the system on a
; variety of arithmetic expressions. Make some intervals A and B, and use them
; in computing the expressions A/A and A/B. You will get the most insight by
; using intervals whose width is a small percentage of the center value.
; Examine the results of the computation in center-percent form (see exercise
; 2.12).

; Here is all the code we will need:

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval x y)
  (cons x y))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (percent i)
  (* (/ (width i) (center i))
     100))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (display-interval leading-text interval)
  (display leading-text)
  (display ": center = ")
  (display (center interval))
  (display ", percent = ")
  (display (percent interval))
  (newline))

; Here are the A and B intervals

(define A (make-interval  99.9 100.1))
(define B (make-interval 199.9 200.1))
(define one (make-interval 1.0 1.0))
(define parallel-resistance (/ 1.0 
                               (+ (/ 1.0 (center A)) 
                                  (/ 1.0 (center B)))))

(display-interval "A" A)
(display-interval "B" B)

(display-interval "par1" (par1 A B))
(display-interval "par2" (par2 A B))

(display "The parallel resistance of the two is ")
(display parallel-resistance)
(newline)

(newline)

; So far we have the following output
; 
; A:    center = 100.0,             percent = 0.09999999999999432
; B:    center = 200.0,             percent = 0.04999999999999716
; par1: center = 66.66679629635391, percent = 0.2166663750004245
; par2: center = 66.66666296296133, percent = 0.08333334166667185
;
; The parallel resistance of the two is 66.66666666666667
;
; We can see that par2 has smaller width and more accurate center. Let's check
; out A/A and A/B

(display-interval "A/A" (div-interval A A))
(display-interval "A/B" (div-interval A B))
(display-interval "A+A" (add-interval A A))

(newline)

; This time we get:
;
; A/A: center = 1.000002000002,     percent = 0.19999980000019435
; A/B: center = 0.5000003750000938, percent = 0.14999992500003134
; A+A: center = 200.0,              percent = 0.09999999999999432
; 
; We see that addition preserves the tolerance in percentage, but
; multiplication and division add them together. Let's take a look at the
; parts of par1 and par2

(display "Let's do par1 first:\n")
(display-interval "AB" (mul-interval A B))
(display-interval "A + B" (add-interval A B))
(display-interval "AB/(A + B)" (div-interval (mul-interval A B)
                                             (add-interval A B)))
(newline)

(display "Now par2:\n")
(display-interval "1/A" (div-interval one A))
(display-interval "1/B" (div-interval one B))
(display-interval "1/A + 1/B" (add-interval (div-interval one A)
                                            (div-interval one B)))
(display-interval "1/(1/A + 1/B)" (par2 A B))
(newline)

; This is the output:
;
; Let's do par1 first:
; AB:            center = 20000.010000000002,    percent = 0.14999992500002837
; A + B:         center = 300.0,                 percent = 0.06666666666666288
; AB/(A + B):    center = 66.66679629635391,     percent = 0.2166663750004245
; 
; Now par2:
; 1/A:           center = 0.010000010000009999,  percent = 0.09999999999999962
; 1/B:           center = 0.0050000012500003126, percent = 0.04999999999999587
; 1/A + 1/B:     center = 0.015000011250010312,  percent = 0.08333334166666921
; 1/(1/A + 1/B): center = 66.66666296296133,     percent = 0.08333334166667185
;
; We can see that we loose precision on every multiplication and division - the
; tolerance in percentage is of both factors is added together. Generally,
; addition decreases the tolerance in percentage whe adding positive numbers
; (that's not entirely true).
;
; In par2 we just do one addition, which decreases the tolerance under 0.01%,
; while in par1 we first do a multiplication and then a division, that gets the
; tolerance up to 0.21%.
;
; And just for a final illustration:

(display-interval "A*A/A" (div-interval (mul-interval A A)
                                        A))

; This resuls to:
;
; A*A/A: center = 100.00040000039999, percent = 0.29999920000237845
;
; The real answer he is A, but the arithmetic gymnastics triple the tolerance.
