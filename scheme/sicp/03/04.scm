; SICP exercise 3.04
;
; Modify the make-account procedure of exercise 3.3 by adding another local
; state variable so that, if an account is accessed more than seven
; consecutive times with an incorrect password, it invokes the procedure
; call-the-cops.

(define (make-account balance account-password)
  (define consecuitive-failed-attempts 0)
  (define (withdraw amount)
    (set! consecuitive-failed-attempts 0)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! consecuitive-failed-attempts 0)
    (set! balance (+ balance amount))
    balance)
  (define (unauthorized amount)
    (set! consecuitive-failed-attempts (+ consecuitive-failed-attempts 1))
    (if (>= consecuitive-failed-attempts 7)
        (call-the-cops)
        #f)
    "Incorrect password")
  (define (dispatch message password)
    (cond ((not (eq? account-password password)) unauthorized)
          ((eq? message 'withdraw) withdraw)
          ((eq? message 'deposit) deposit)
          (else (error "Unknown request - MAKE-ACCOUNT" m))))
  dispatch)
