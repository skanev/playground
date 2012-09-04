; SICP exercise 3.74
;
; Alyssa P. Hacker is designing a system to process signals coming from
; physical sensors. One important feture she wishes to produce is a signal
; that describes the zero corssings of the input signal. That is, the
; resulting singal should be +1 whenever the input signal changes form
; negative to positive, -1 whenever the input signal changes from positive to
; negative and 0 otherwise. (Assume that the sign of 0 input is positive.) For
; example, a typical input signal with its associated zero-crossing signal
; would be:
;
;   ...1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4...
;   ...0  0   0   0   0    -1    0   0   0    0    1   0  0...
;
; In Alyssa's system, the signal from the sensor is represented as a stream
; sense-data and the stream zero-crossings is the corresponding stream of zero
; crossings. Alyssa first writes a procedure sign-change-detector that takes
; two values as arguments and compares the signs of the values to produce an
; appropriate 0, 1, or -1. She then constructs her zero-crossing stream as
; follows:
;
;   (define (make-zero-crossings input-stream last-value)
;     (cons-stream
;       (sign-change-detector (stream-car input-stream)
;                             last-value)
;       (make-zero-crossings (stream-cdr input-stream)
;                            (stream-car input-stream))))
;
;   (define zero-crossings (make-zero-crossings sense-data 0))
;
; Alyssa's boss, Eva Lu Ator, walks by and suggests that this program is
; approximately equivalent to the following one, which uses the generalized
; version of stream-map from exercise 3.50
; 
;   (define zero-crossings
;     (stream-map sign-change-detector sense-data <expression>))
;
; Complete the program by supplying the indicated <expression>.

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))

(define (stream-take stream n)
  (if (= n 0)
      '()
      (cons (stream-car stream) (stream-take (stream-cdr stream) (- n 1)))))

(define (sign-change-detector a b)
  (cond ((and (< b 0) (< 0 a)) 1)
        ((and (< a 0) (< 0 b)) -1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (stream-cons
    (sign-change-detector (stream-car input-stream)
                          last-value)
    (make-zero-crossings (stream-cdr input-stream)
                         (stream-car input-stream))))

(define (make-zero-crossings-with-map sense-data)
  (stream-map2 sign-change-detector sense-data (stream-cons 0 sense-data)))
