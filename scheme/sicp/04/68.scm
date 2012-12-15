; SICP exercise 4.68
;
; Define rules to implement the reverse operation of exercise 2.18, which
; returns a list containing the same elements as a given list in reverse
; order. (Hint: Use append-to-form.) Can your rules answer both
; (reverse (1 2 3) ?x) and (reverse ?x (1 2 3))?

; They can't. (reverse ?x (1 2 3)) gets the system stuck in an infinite loop.

(add-to-data-base!
  '((rule (reverse (?x) (?x)))
    (rule (reverse (?a . ?b) ?c)
          (and (reverse ?b ?r-b)
               (append-to-form ?r-b (?a) ?c)))))
