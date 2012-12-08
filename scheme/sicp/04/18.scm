; SICP exercise 4.18
;
; Consider an alternative strategy for scanning out definitions that
; translates the example in the text to
;
;   (lambda <vars>
;     (let ((u '*unassigned*)
;           (v '*unassigned*))
;       (let ((a '<e1>)
;             (b '<e2>))
;         (set! u a)
;         (set! v b))
;       <e3>))
;
; Here a and b are meant to represent new variables names, created by the
; interpreter, that do not appear in the user's program. Consider the solve
; procedure from 3.5.4:
;
;   (define (solve f y0 dt)
;     (define y (integral (delay dy) y0 dt))
;     (define dy (stream-map f y))
;     y)
;
; Will this procedure work if internal definitions are scanned out as shown in
; this exercise? What if they are scanned out as shown in the text? Explain.

; In the first case, the result will be:
;
;   (lambda (f y 0 dt)
;     (let ((y '*unassigned*)
;           (dy '*unassigned*))
;       (let ((a (integral (delay dy) y0 dt))
;             (b (stream-map f y)))
;         (set! y a)
;         (set! dy b))
;       y))
;
; This will not work, because b will be (stream-map f '*unassigned*).
;
; In the second case, the expansion will produce:
;
;   (lambda (f y 0 dt)
;     (let ((y '*unassigned)
;           (dy '*unassigned))
;       (set! y (integral (delay dy) y0 dt))
;       (set! dy (stream-map f y))
;       y))
;
; This will work, since dy will refer to the proper value of y.
