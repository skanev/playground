; SICP exercise 2.11
;
; In passing, Ben also cryptically comments: "By testing the signs of the
; endpoints of the intervals, it is possible to break mul-interval into nine
; cases, only one of which requires more than two multiplications." Rewrite
; this procedure using Ben's suggestion.

; Let's have (a, b) x (c, d).
;
; We know that a ≤ b and c ≤ d. Thus, we have the following cases:
;
; +---+---+---+---+----------------------------+
; | a | b | c | d | result                     |
; +---+---+---+---+----------------------------+
; | + | + | + | + | (ac, bd)                   |
; +---+---+---+---+----------------------------+
; | + | + | - | + | (bc, bd)                   |
; +---+---+---+---+----------------------------+
; | + | + | - | - | (bc, ad)                   |
; +---+---+---+---+----------------------------+
; | - | + | + | + | (ad, bd)                   |
; +---+---+---+---+----------------------------+
; | - | + | - | + | (min(ad, bc), max(ac, bd)) |
; +---+---+---+---+----------------------------+
; | - | + | - | - | (bc, ac)                   |
; +---+---+---+---+----------------------------+
; | - | - | + | + | (ad, bc)                   |
; +---+---+---+---+----------------------------+
; | - | - | - | + | (ad, ac)                   |
; +---+---+---+---+----------------------------+
; | - | - | - | - | (bd, ac)                   |
; +---+---+---+---+----------------------------+
;
; Hence, the code

(define (mul-interval x y)
  (define (pos? number)
    (<= 0 number))
  (define (neg? number)
    (<= number 0))
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((and (pos? a) (pos? b) (pos? c) (pos? d)) (make-interval (* a c) (* b d)))
          ((and (pos? a) (pos? b) (neg? c) (pos? d)) (make-interval (* b c) (* b d)))
          ((and (pos? a) (pos? b) (neg? c) (neg? d)) (make-interval (* b c) (* a d)))
          ((and (neg? a) (pos? b) (pos? c) (pos? d)) (make-interval (* a d) (* b d)))
          ((and (neg? a) (pos? b) (neg? c) (pos? d))
           (make-interval (min (* a d) (* b c)) (max (* a c) (* b d))))
          ((and (neg? a) (pos? b) (neg? c) (neg? d)) (make-interval (* b c) (* a c)))
          ((and (neg? a) (neg? b) (pos? c) (pos? d)) (make-interval (* a d) (* b c)))
          ((and (neg? a) (neg? b) (neg? c) (pos? d)) (make-interval (* a d) (* a c)))
          ((and (neg? a) (neg? b) (neg? c) (neg? d)) (make-interval (* b d) (* a c))))))

(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))
