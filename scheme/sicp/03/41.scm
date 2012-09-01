; SICP exercise 3.41
;
; Ben Bitdiddle worries that it would be better to implement the bank account
; as follows (where the commented line has been changed):
;
;   (define (make-account balance)
;     (define (withdraw amount)
;       (if (>= balance amount)
;           (begin (set! balance (- balance amount))
;                  balance)
;           "Insufficient funds"))
;     (define (deposit amount)
;       (set! balance (+ balance amount))
;       balance)
;     (let ((protected (make-serializer)))
;       (define (dispatch m)
;         (cond ((eq? m 'withdraw) (protected withdraw))
;               ((eq? m 'deposit) (protected deposit))
;               ((eq? m 'balance) (protected (lambda () balance)))
;               (else (error "Unknown request - MAKE-ACCOUNT" m))))
;       dispatch))
;
; because allowing unserialized access to the bank balance can result in
; anomalous behavior. Do you agree? Is there any scenario that demonstrats
; Ben's concern?

; In general - no. It works perfectly fine without serializing the access.
;
; In case we're working with some ancient technology where writes to the
; memory are not atomic, balance might be read while withdraw or deposit are
; in the middle of updating in, thus we can get a jumble of bits that is not
; the real balance. Finding a machine where that could happen in the present
; year, though, might be very expensive.
