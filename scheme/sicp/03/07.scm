; SICP exercise 3.07
;
; Consider the bank account objects created by make-account, with the password
; modification described in exercise 3.3. Suppose that our banking system
; requires the ability to make joint accounts. Define a procedure make-joint
; that accomplishes this. make-joint should take three arguments. The first is
; a password-protected account. The second argument must match the password
; with which the account was defined in order for the make-joint operation to
; proceed. The third argument is a new password. make-joint is to create an
; additional access to the original account using the new password. For
; example, if peter-acc is a bank account with password open-sesame, then
;
; (define paul-acc
;   (make-joint peter-acc 'open-sesame 'rosebud))
;
; will allow one to make transaction on peter-acc using the name paul-acc and
; the password rosebud. You may wish to modify your solution to exercise 3.3
; to accommodate for this new feature.

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

(define (make-joint account original-password new-password)
  (lambda (message password)
    (if (eq? password new-password)
        (account message original-password)
        (lambda (amount) "Incorrect password"))))
