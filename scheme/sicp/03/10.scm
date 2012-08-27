; SICP exercise 3.10
;
; In the make-withdraw procedure, the local variable balance is created as a
; parameter of make-withdraw. We could also create the local state variable
; explicitly, using let, as follows:
;
;   (define (make-withdraw initial-amount)
;     (let ((balance initial-amount))
;       (lambda (amount)
;         (>= balance amount)
;         (being (set! balance (- balance amount))
;                (balance)
;         "Insufficient funds"))))
;
; Recall from section 1.3.2 that let is simply syntactic sugar for a procedure
; call:
;
;   (let ((<var> <exp>)) <body)
;
; is interpreted as an alternate syntax for:
;
;   ((lambda (<var>) <body>) <exp)
;
; Use the environment model to analyze this alternate version of
; make-withdraw, drawing figures like the ones above to illustrate the
; interactions
;
;   (define W1 (make-withdraw 100))
;
;   (W1 50)
;
;   (define W2 (make-withdraw 100))
;
; Show that the two versions of make-withdraw create objects with the same
; behavior. How do environment structures differ in the two versions?

; Alright, here's the diagram before (W1 50)
;
; global-env: +-----------------------------------------------------------------------+
;             | W1                                                                    |
;             +-|---------------------------------------------------------------------+
;               |  +---------------------+
;               |  | initial-amount: 100 |
;               |  +---------------------+
;               |            ^
;               |            |
;               |  +---------------------+
;               |  | amount: 100         |
;               |  +---------------------+
;               |            ^
;               |            |
;             +--------+     |
;             | lambda |-----+
;             +--------+
;               |
;               parameters: amount
;               body: (if (>= balance amount)
;                         (begin (set! balance (- balance amount))
;                                balance)
;                         "Insufficient funds"))
;
; After the withdraw, we get the following:
;
; global-env: +-----------------------------------------------------------------------+
;             | W1                             W2                                     |
;             +-|------------------------------|--------------------------------------+
;               |  +---------------------+     |  +---------------------+
;               |  | initial-amount: 100 |     |  | initial-amount: 100 |
;               |  +---------------------+     |  +---------------------+
;               |            ^                 |            ^
;               |            |                 |            |
;               |  +---------------------+     |  +---------------------+
;               |  | amount: 50          |     |  | amount: 100         |
;               |  +---------------------+     |  +---------------------+
;               |            ^                 |            ^
;               |            |                 |            |
;             +--------+     |               +--------+     |
;             | lambda |-----+               | lambda |-----+
;             +--------+                     +--------+
;               |                              |
;               parameters: amount             parameters: amount
;               body: ...                      body: ...
;
; The difference is that this version creates one additional frame that holds
; the amount. That way the initial amount is still available, although not
; used in the code we currently have. Since the initial-amount is not used, we
; can safely ignore it. Since this version is only modifying the second frame
; in a manner, similar to the previous version, the behavior of the two
; version is the same.
