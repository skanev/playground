; SICP exercise 4.65
;
; Cy D. Fect, looking forward to the day when he will rise in the
; organization, gives a query to find all the wheels (using the wheel rule of
; section 4.4.1):
;
; (wheel ?who)
;
; To his surprise, the system responds
;
; ;;; Query results:
; (wheel (Warbucks Oliver))
; (wheel (Bitdiddle Ben))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
;
; Why is Oliver listed four times?

; The rule looks like this:
;
;   (rule (wheel ?person)
;         (and (supervisor ?middle-manager ?person)
;              (supervisor ?x ?middle-manager)))
;
; With this definition, it lists the wheel once for each employee supervised
; by a middle manager under them. Alyssa, Lem and Cy are under Ben, while
; Robert is under Eben. This is why Oliver is listed four times. The only
; supervisor under Ben is Alyssa and she is supervising one employee - Louis,
; which is why Ben is listed once.
;
; The order is not as straightfoward, since stream-flatmap interleaves the
; streams it maps to.
