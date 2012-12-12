; SICP exercise 4.63
;
; The following data base (see Genesis 4) traces the genealogy of the
; descendants of Ada back to Adam, by way of Cain:
;
; (son Adam Cain)
; (son Cain Enoch)
; (son Enoch Irad)
; (son Irad Mehujael)
; (son Mehujael Methushael)
; (son Methushael Lamech)
; (wife Lamech Ada)
; (son Ada Jabal)
; (son Ada Jubal)
;
; Formulate rules such as "If S is the son of F, and F is the son of G, then S
; is the grandson of G" and "If W is the wife of M, and S is the son of W,
; then S is the son of M" (which was supposedly more true in biblical times
; than today) that will enable to query the system to find grandson of Cain;
; the sons of Lamech; the grandsons of Methushael. (See exercise 4.69 for some
; rules to deduce more complicated relationships.)

(add-to-data-base!
  '((son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)

    (rule (grandson ?grandfather ?son)
          (and (son ?grandfather ?father)
               (son ?father ?son)))

    (rule (son ?father ?son)
          (and (wife ?father ?mother)
               (son ?mother ?son)))))

(define grandson-of-cain '(grandson Cain ?grandson))
(define sons-of-lamech '(son Lamech ?son))
(define grandsons-of-methushael '(grandson Methushael ?grandson))
