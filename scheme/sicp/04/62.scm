; SICP exercise 4.62
;
; Define rules to implement the last-pair operation of exercise 2.17, which
; returns a list containing the last element of a nonempty list. Check your
; rules on queries such as (last-pair (3) ?x), (last-pair (1 2 3) ?x), and
; (last-pair (2 ?x) (3)). Do your rules work correctly on queries such as
; (last-pair ?x (3)).

; The rule gets stuck in an infinite recursion. This is to be expected, since
; there are infinitely many pairs whose last pair is (3). That is to say, they
; don't work.

(add-to-data-base!
  '((rule (last-pair (?x) (?x)))
    (rule (last-pair (?any . ?x) ?y)
          (last-pair ?x ?y))))
