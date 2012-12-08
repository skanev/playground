; SICP exercise 4.27
;
; Suppose we type in the following definitions to the lazy evaluator:
;
;   (define count 0)
;   (define (id x)
;     (set! count (+ count 1))
;     x)
;
; Give the missing values in the following sequence of interactions, and
; explain your answers:
;
; (define w (id (id 10)))
; ;;; L-Eval input
; count
; ;;; L-Eval output
; <response>
; ;;; L-Eval input
; w
; ;;; L-Eval output
; <response>
; ;;; L-Eval input
; count
; ;;; L-Eval output
; <response>

; Here is the full interaction:
;
; (define w (id (id 10)))
; ;;; L-Eval input
; count
; ;;; L-Eval output
; 1
; ;;; L-Eval input
; w
; ;;; L-Eval output
; 10
; ;;; L-Eval input
; count
; ;;; L-Eval output
; 2
;
; When we define w, id gets executed. It takes (id 10) for an argument,
; invokes set! to increment the count and returns a thunk containing (id 10).
; At this point, when we print out count, we get 1.
;
; When we print w, the REPL forces the thunk which calls id again with 10 as
; an argument. set! is invoked one more, incrementing counter to 2. The result
; is printed.
;
; The subsequent printing of count shows the twice incremented value.
