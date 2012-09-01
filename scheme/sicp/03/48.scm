; SICP exercise 3.48
;
; Explain in detail why the deadlock-avoidance method described above, (i.e.
; accounts are numbered, and each process attempts to acquire the
; smaller-numbered account first) avoids deadlock in the exchange problem.
; Rewrite serialzied-exchange to incorporate this idea. (You will also need to
; modify make-account so that each account is created with a number, which can
; be accessed by sending an appropriate message.)

; In order to have a deadlock, we need two processes and are attempting to
; require locks on two resources (x and y). The situation can occur if one
; tries to acquire the locks in order (x, y) and the other in order (y, x). If
; both manage to acquire the first lock before they attempt to require the
; second, a deadlock occurs.
;
; If we order the accounts and acquire locks only in ascending order, this
; situation cannot arise, because both will attempt to acquire the locks in
; order (x, y).
;
; Here's some untested code:

; First, we need a way to generate account numbers. We need to have a lock for
; this, otherwise we might end up with two accounts sharing the same number.

(define last-account-number 0)
(define account-number-lock (make-mutex))
(define (generate-account-number)
  (account-number-lock 'acquire)
  (let ((number last-account-number))
    (set! last-account-number (+ last-account-number 1))
    (account-number-lock 'release)
    number))

; This is how we make accounts with a number:

(define (make-account-and-serializer balance)
  (let ((number (generate-account-number))
        (serializer (make-serializer)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) serializer)
            ((eq? m 'number) number)
            (else (error "Unknown request - MAKE-ACCOUNT" m))))
      dispatch))

; This is how our serialized-exchange looks like:

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serialize-both exchange acount1 acount2) account1 account2)))

; And this is or auxiliary function. The outer serializer gets invoked first.
; Note, that it doesn't matter whether we lock accounts in ascending or
; descending order, as long as we always lock in the same order.

(define (serialize-both proc account1 account2)
  (if (< (number account1) (number account2))
      ((account1 (account2 proc)))
      ((account2 (account1 proc)))))
