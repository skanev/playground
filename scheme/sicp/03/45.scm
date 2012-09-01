; SICP exercise 3.45
;
; Louis Reasoner thinks our bank-account system is unnecessarily complex and
; error-prone now that deposits and withdrawals aren't automatically
; serialized. He suggests and make-account-and-serializer should have exported
; the serializer (for use by such procedures as serialized-exchange) in
; addition to (rather than instead of) using it to serialize accounts and
; deposists as make-account did. He proposes to redefine accounts as follows:
;
;   (define (make-account-and-serializer balance)
;     (define (withdraw amount)
;       (if (>= balance amount)
;           (begin (set! balance (- balance amount))
;                  balance)
;           "Insufficient funds"))
;     (define (deposit amount)
;       (set! balance (+ balance amount))
;       balance)
;     (let ((balance-serialzier (make-serializer)))
;       (define (dispatch m)
;         (cond ((eq? m 'withdraw) (balance-serialzier withdraw))
;               ((eq? m 'deposit) (balance-serialzier deposit))
;               ((eq? m 'balance) balance)
;               ((eq? m 'serializer) balance-serialzier)
;               (else (error "Unknown request - MAKE-ACCOUNT" m))))
;       dispatch))
;
; Then deposits are handled as with the original make-account:
;
;   (define (deposit account amount)
;     ((acount 'deposit) amount))
;
; Explain what is wrong with Louis' reasoning. In particular, consider whan
; happens when serialized-exchange is called.

; The system will deadlock.
;
; When se called serialized-exchange, it will use the serializer of an account
; to execute the exchange procedure. However, exchange will eventually call
; withdraw, which in turn uses the same serializer to modify the amount.
; However, it cannot proceed until exchange is finished - and exchange cannot
; finish until (at least) the withdrawal completes.
