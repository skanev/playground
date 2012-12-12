; SICP exercise 4.61
;
; The following rules implement a next-to relation that finds adjancent
; elements of a list.
;
; (rule (?x next-to ?y in (?x ?y . ?u)))
;
; (rule (?x next-to ?y in (?v . ?z))
;       (?x next-to ?y in ?z))
;
; What will the response be to the following queries?
;
; (?x next-to ?y in (1 (2 3) 4))
; (?x next-to 1 in (2 1 3 1))

(add-to-data-base!
 '((rule (?x next-to ?y in (?x ?y . ?u)))
   (rule (?x next-to ?y in (?v . ?z))
         (?x next-to ?y in ?z))))

(define query-1 '(?x next-to ?y in (1 (2 3) 4)))
(define query-2 '(?x next-to 1 in (2 1 3 1)))

(define response-1 '((1 next-to (2 3) in (1 (2 3) 4))
                     ((2 3) next-to 4 in (1 (2 3) 4))))

(define response-2 '((2 next-to 1 in (2 1 3 1))
                     (3 next-to 1 in (2 1 3 1))))
