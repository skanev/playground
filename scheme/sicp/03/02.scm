; SICP exercise 3.02
;
; In software-testing applications, it is useful to be able to count the
; number of times a given procedure is caled during the course of a
; computation. Write a procedure make-monitored that takes as input a
; procedure, f, that itself takes one input.The result returned by
; make-monitored is a third procedure, say mf, that keeps track of the number
; of times is has been called by a maintaining an internal counter. If the
; input to mf is the special symbol how-many-calls?, then mf returns the value
; of the counter. If the input is the special symbol reset-count, then mf
; resets the counter to zero. For any other input, mf returns the result of
; calling f on that input and increments the counter. For instance, we sould
; make a monitored version of the sqrt procedure:
;
; (define s (make-monitored sqrt))
;
; (s 100)
; 10
;
; (s 'how-many-calls?)
; 1

(define (make-monitored function)
  (let ((count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) count)
            ((eq? arg 'reset-count) (set! count 0))
            (else (set! count (+ count 1))
                  (function arg))))))
