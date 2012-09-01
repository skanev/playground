; SICP exercise 3.42
;
; Ben Bitdiddle suggests that it's a waste of time to create a new serialized
; procedure in response to every withdraw and deposit message. He says that
; make-account could be changed so that the calls to protected are done
; outside the dispatch procedure. That is, an account would return the same
; serialized procedure (which was created at the same time as the account)
; each time it is asked for a withdrawal procedure.
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
;       (let ((protected-withdraw (protected withdraw))
;             (protected-deposit (protected deposit)))
;         (define (dispatch m)
;           (cond ((eq? m 'withdraw) protected-withdraw)
;                 ((eq? m 'deposit) protected-deposit)
;                 ((eq? m 'balance) balance)
;                 (else (error "Unknown request - MAKE-ACCOUNT" m))))
;         dispatch)))
;
; Is this a safe change to make? In particular, is there any difference in
; what concurrency is allowed by these two versions of make-account?

; It actually depends on how make-serializer is implemented. In the original
; version, two withdrawals are two separate procedures that are serialized. In
; Ben's approach, we have one serialized procedure that can be called twice at
; the same time. The question is if the serializer executes those concurrently
; or sequentially.
;
; If execution is sequential (as it should be, if we're using the serializer
; implementation later in the chapter), then the change is not a problem. On
; the other hand, if it is a weird serializer that allows the function to be
; executed twice concurrently, then it is a problem. But I don't think it's
; likely for a serializer to be like that.
