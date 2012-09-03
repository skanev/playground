; SICP exercise 3.64
;
; Write a procedure stream-limit that takes as arguments a stream and a number
; (the tolerance). It should examine the stream until it finds two successive
; elements that differ in absolute value by less than the tolerance, and
; return the second of the two elements. Using this, we could compute square
; roots up to a given tolerance by
;
;   (define (sqrt x tolerance)
;     (stream-limit (sqrt-stream x) tolerance))

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (average a b)
  (/ (+ a b)
     2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream))))
    (if (< (abs (- s1 s2))
           tolerance)
        s2
        (stream-limit (stream-cdr stream) tolerance))))

(define (sqrt-tolerance number tolerance)
  (stream-limit (sqrt-stream number) tolerance))
