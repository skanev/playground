; SICP exercise 3.14
;
; The following procedure is quite useful, although obscure:
;
;   (define (mystery x)
;     (define (loop x y)
;       (if (null? x)
;           y
;           (let ((temp (cdr x)))
;             (set-cdr! x y)
;             (loop temp x))))
;     (loop 'x ()))
;
; loop uses the "temporary variable temp to hold the old value of the cdr of
; x, since the set-cdr! on the next line destroys the cdr. Explain what
; mystery does in general. Suppose v is defined by
;
;   (define v (list 'a 'b 'c 'd))
;
; Draw the box-and-pointer diagram that represents the list to which v is
; bound. Suppose that we now evaluate (define w (mystery v)). Draw
; box-and-pointer diagrams that show the structures v and w after evaluating
; this expression. What would be printed as the values of v and w?

; Here's the box and pointer diagram for v:
;
;        +---+---+    +---+---+    +---+---+    +---+---+
; v ---> | . | . -----| . | . -----| . | . -----| . | / |
;        +-|-+---+    +-|-+---+    +-|-+---+    +-|-+---+
;          |            |            |            |
;        +---+        +---+        +---+        +---+
;        | a |        | b |        | c |        | d |
;        +---+        +---+        +---+        +---+
;
; This is what happens after we call mystery:
;
;          +---------------+ +----------+ +----------------+
;          |               | |          | |                |
;        +---+---+    +---+|--+    +---+|--+         +---+-|-+
; v ---> | . | / |    | . | . |    | . | . |  w ---> | . | . |
;        +-|-+---+    +-|-+---+    +-|-+---+         +-|-+---+
;          |            |            |                 |
;        +---+        +---+        +---+             +---+
;        | a |        | b |        | c |             | d |
;        +---+        +---+        +---+             +---+
;
; In general, mystery reverses the list in space. It sets the cdr of the
; second element to the first, then the cdr of the third element to the second
; and so on. When it runs out of elements, it returns the last.
;
; In the end, the values are:
;
;   w: (d c b a)
;   v: (a)
