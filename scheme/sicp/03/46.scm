; SICP exercise 3.46
;
; Suppose that we implement test-and-set! using an ordinary procedure as shown
; in the text, without attempting to make the operation attomic. Draw a timing
; diagram like the one in figure 3.29 to demonstrate how the mutex
; implementation can fail by allowing two processes to acquire the mutex at
; the same time.

; Eh.
;
; (the-mutex 'acquire)                  (the-mutex 'acquire)
; ------------------------------------- -------------------------------------
; test-and-set! cell
;                                       test-and-set! cell
;   (if (car cell)
;                                       (if (car cell)
;   (begin (set-car! cell true)
;          false)
;
;                                       (begin (set-car! cell true)
;                                              false)
;     (set-car! cell true)
;     false
;                                         (set-car! cell true)
;                                         false
;
; Both calls set the variable to false and then return immediatelly. Now the
; mutex has been acquired by two separate processes.
