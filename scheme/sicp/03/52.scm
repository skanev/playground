; SICP exercise 3.52
;
; Consider the sequence of expressions
;
;   (define sum 0)
;   (define (accum x)
;     (set! sum (+ x sum))
;     sum)
;
;   (define seq
;     (stream-map accum
;                 (stream-enumerate-interval 1 20)))
;
;   (define y (stream-filter even? seq))
;   (define z (stream-filter
;               (lambda (x) (= (remainder x 5) 0)) seq))
;
;   (stream-ref y 7)
;   (display-stream z)
;
; What is the value of sum after each of the above expressions is evaluated?
; What is the printed response to evaluating the stream-ref and display-stream
; expressions? Would these responses differ if we had implemented (delay
; <exp>) simply as (lambda () <exp>) without using the optimization provided
; by memo-proc? Explain.

; Fully evaluated, seq is:
;
; (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
;
; However, it gets evaluated in parts. Here is how it goes:
;
; (define seq
;   (stream-map accum
;               (stream-enumerate-interval 1 20)))
; sum = 1
;
; (define y (stream-filter even? seq))
; sum = 6
;
; (define z (stream-filter
;             (lambda (x) (= (remainder x 5) 0)) seq))
; sum = 10
;
; (stream-ref y 7)
; sum = 136
; result = 136
;
; (display-stream z)
; sum = 210
; output is:
;   10
;   15
;   45
;   55
;   105
;   120
;   190
;   210
;
; If we did not memoize the delayed thunk, each subsequent interation of the
; stream would modify the sum. Iterating seq twice would produce different
; results. For, after (define y ...) sum will be 7 and after (define z ...),
; sum will be 17. Each evaluation of a part of the stream will offset the
; elements more and more.
;
; Without this optimization, we cannot iterate a stream twice if there are
; side effects in the stream.
