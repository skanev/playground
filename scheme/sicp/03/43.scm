; SICP exercise 3.43
;
; Suppose that the balances in three accounts start out as $10, $20, and $30,
; and that multiple processes run, exchanging the balances of the accounts.
; Argue that if the processes are run sequentially, after any number of
; concurrent exchanges, the account balances should be $10, $20 and $30 in
; some order. Draw a timing diagram like the one in figure 3.29 to show how
; this condition can be violated if the exchanges are implemented using the
; first version of the account-exchange program in this section. On the other
; hand, argue that even with this exchange program, the sum of the balances in
; the accounts will be preserved. Draw a timing diagram to show how even this
; condition would be violated if we did not serialize the transactions on
; individual accounts.

; Well, exchanging x and y in (x, y, z) will result to (y, x, z). It is the
; same for any other pair. The numbers are preserved, even if the order is
; changed.
;
; (x, y, z) = (10, 20, 30) ; 60 total
;
; (exchange y z)                  (exchange z x)
; ------------------------------- -------------------------------
; read y: 20
; read x: 10
; difference: 10
;
;                                 read z: 30
;                                 read x: 10
;                                 difference: 20
; withdraw y 10: 10
;                                 withdraw z 20: 10
; deposit x 10: 20
;                                 deposit x 20: 40
;
; When the exchanges complete, the account balances will be (40, 10, 10) that
; ammounts to 60 total. In general, once the difference is calculated, when it
; is removed to one account it will be added to another. There is no loss of
; money, even if the balances end up randomly.
;
; If we use the non-serialized access to the accounts, we can easily get this
; situation:
;
; (exchange y z)                  (exchange z x)
; ------------------------------- -------------------------------
; read y: 20
; read x: 10
; difference: 10
;
;                                 read z: 30
;                                 read x: 10
;                                 difference: 20
; withdraw y 10
;   read y: 20
;   set y: 10
;                                 withdraw z 20
;                                   read z: 30
;                                   set z: 10
; deposit x 10
;   read x: 10
;                                 deposit x 20
;                                   read x: 10
;                                   set x: 30
;   set x: 20
;
; In the end, we get (20, 10, 10) which is a loss of money. Not using a
; serializer just costed the bank 20 bucks.
