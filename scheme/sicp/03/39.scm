; SICP exercise 3.39
;
; Which of the five possibilities in the parallel execution shown above remain
; if we instead serialize exection as follows:
;
;   (define x 10)
;
;   (define s (make-serializer))
;
;   (parallel-execute
;     (lambda () (set! x ((s (lambda () (* x x))))))
;     (s (lambda () (set! x (+ x 1)))))

; If we assume that the lambdas are P1 and P2, then the possible values are:
;
;   101: P1 sets x to 100 and then P2 increments x to x.
;   121: P2 increments x to 11 and then P2 sets x to x.
;   100: P1 accesses x, then P2 sets X to 11, then P1 sets x.
