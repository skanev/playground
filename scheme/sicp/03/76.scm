; SICP exercise 3.76
;
; Eva Lu Ator has a criticism of Louis's approach in exercise 3.75. The
; program he wrote is not modular, because it intermixes the operation of
; smoothing with the zero-crossing extraction. For example, the extractor
; should not have to be changed if Alyssa finds a better way to condition her
; input signal. Help Louis by writing a procedure smooth that takes a stream
; as input and produces a stream in which each element is the average of two
; succesive input stream elements. Then use smooth as a component to implement
; the zero-crossing detector in a more modular style.

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

(define (smooth stream)
  (define (smooth-stream stream prev)
    (let ((first (stream-car stream))
          (rest (stream-cdr stream)))
      (stream-cons (/ (+ first prev) 2.0)
                   (smooth-stream rest prev))))
  (stream-cons (stream-car stream)
               (smooth-stream (stream-cdr stream) (stream-car stream))))

(define (make-zero-crossings input-stream last-value)
  (let ((smooth-stream (smooth input-stream)))
    (stream-map2 sign-change-detector smooth-stream (stream-cons 0 smooth-stream))))
