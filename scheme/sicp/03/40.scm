; SICP exercise 3.40
;
; Give all possible values of x that can result from executing
;
;   (define x 10)
;
;   (parallel-execute (lambda () (set! x (* x x)))
;                     (lambda () (set! x (* x x x))))
;
; Which of these possibilities remain if we instead use serialized procedures:
;
;   (define x 10)
;
;   (define s (make-serializer))
;
;   (parallel-execute (s (lambda () (set! x (* x x))))
;                     (s (lambda () (set! x (* x x x)))))

; Assuming that (* x x) and (* x x x) is atomic and reads the same x all
; times and that the lambdas are P1 and P2:
;
;   1000000: P1 reads (10) and sets (100), P2 reads (100) and sets (1000000)
;   1000000: P2 reads (10) and sets (1000), P1 reads (1000) and sets (1000000)
;      1000: P1-read (10), P2-read (10), P1-set (100), P2-set (1000)
;       100: P1-read (10), P2-read (10), P2-set (1000), P1-set (100)
;      1000: P2-read (10), P1-read (10), P1-set (100), P2-set (1000)
;       100: P2-read (10), P1-read (10), P2-set (1000), P1-set (100)
;
; If we use the serializer, only the first two option remain.
