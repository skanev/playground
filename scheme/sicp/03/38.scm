; SICP exercise 3.38
;
; Suppose that Peter, Paul and Mary share a joint back account that initially
; contains $100. Concurrently, Peter deposits $10, Paul withdraws $20, and
; Mary withdraws half the money in the account, by executing the following
; commands:
;
;   Peter: (set! balance (+ balance 10))
;   Paul:  (set! balance (- balance 20))
;   Mary:  (set! balance (- balance (/ balance 2)))
;
; a. List all the different possible values for balance after these three
; transactions have been completed, assuming that the banking system forces
; the three processes to run sequentially in some order.
;
; b. What are some other values that could be produced if the system allows
; the processes to be interleaved? Draw timing diagrams like the one in figure
; 3.29 to explain how these values can occur.

; a. The possible values are 35, 40, 45 and 50. To illustrate them, we will
; abbreviate the operations to +10, -20 and /2. Here are the options:
;
;   45: +10, -20, /2
;   35: +10, /2, -20
;   45: -20, +10, /2
;   50: -20, /2, +10
;   40: /2, +10, -20
;   40: /2, -20, +10
;
; b. I hate plurals. One possible value is 30.
;
;   Peter                   Paul                    Marry
;   ----------------------- ----------------------- -----------------------
;                                                   access balance: 100
;                                                   new value: 50
;                                                   set balance: 50
;                           access balance: 50
;   access balance: 50
;   new value: 70
;   set balance: 70
;                           new value: 30
;                           set balance: 30
;
; Another possible value is 55:
;
;   Peter                   Paul                    Marry
;   ----------------------- ----------------------- -----------------------
;   access balance: 100
;                           access balance: 100
;   new value: 110
;                           new value: 80
;                           set balance: 80
;   set balance: 110
;                                                   access balance: 110
;                                                   new value: 55
;                                                   set balance: 55
