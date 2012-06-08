; SICP exercise 3.06
;
; It is useful to be able to reset a random-number generator to produce a
; sequence starting from a given value. Design a new rand procedure that is
; called with an argument that is either the symbol generate or the symbol
; reset and behaves as follows: (rand 'generate) produces a new random number;
; ((rand 'reset) <new-value>) resets the internal state variable to the
; designated <new-value>. Thus, by resetting the state, one can generate
; repeateble sequences. These are very handy to have when testing and
; debugging programs that use random numbers.

(define seed (current-milliseconds))

(define (rand-update number)
  (let ((modulus 4294967296)
        (multiplier 1664525)
        (increment 1013904223))
    (modulo (+ (* multiplier number) increment) modulus)))

(define rand
  (let ((x seed))
    (lambda (message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))
            (else (error "Unknown request - RAND" message))))))
