; SICP exercise 3.49
;
; Give a scenario where the deadlock-avoidance mechanism described above does
; not work. (Hint: In the exchange problem, each process knows in advance
; which accounts it will neet to get access to. Consider a situation where a
; process must get access to some shared resource before it can know which
; additional shared resources it will require.)

; The question pretty much answers itself. Let's say that we need to acquire
; two locks. We need to acquire the first lock in order to determine what
; second lock we need to acquire later. Let's say we need to acquire a in
; order to determine that we need to acquire b second. If the reverse case is
; possible (we acquire b and then we determine that we need to acquire a
; second), there is a possibility of a deadlock.
;
; We can solve this problem by having a third lock we acquire before acquiring
; the first one.
