; SICP exercise 4.73
;
; Why does flatten-stream use delay explicitly? What would be wrong with
; defining it as follows:
;
; (define (flatten-stream stream)
;   (if (stream-null? stream)
;       the-empty-stream
;       (interleave
;         (stream-car stream)
;         (flatten-stream (stream-cdr stream)))))

; Well, to be fair, this is just how I implemented it. If it is defined that
; way, flatten-stream would not terminate if the stream is infinite.
