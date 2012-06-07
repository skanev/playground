; SICP exercise 3.03
;
; Modify the make-account procedure so that is creates password-protected
; accounts. That is, make-account should take a sumbol as an additional
; argument, as in:
;
;   (define acc (make-account 100 'secret-password))
;
; The resulting account object should process a request only if it is
; accompanied by the password with which the account was created, and should
; otherwise return a complaint:
;
;   ((acc 'secret-password 'withdraw) 40)
;   60
;
;   ((acc 'some-other-password 'deposit) 50)
;   "Incorect password"

(define (make-account balance account-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (unauthorized amount)
    "Incorrect password")
  (define (dispatch message password)
    (cond ((not (eq? account-password password)) unauthorized)
          ((eq? message 'withdraw) withdraw)
          ((eq? message 'deposit) deposit)
          (else (error "Unknown request - MAKE-ACCOUNT" m))))
  dispatch)
