; SICP exercise 4.60
;
; By giving the query
;
; (lives-near ?person (Hacker Alyssa P))
;
; Alyssa P. Hacker is able to find people who live near her, with whom she can
; ride to work. On the other hand, when she tries to find all pairs of people
; who live near each other by querying
;
; (lives-near ?person-1 ?person-2)
;
; she notices that each pair of people who live near each other is listed
; twice; for example,
;
; (lives-near (Hacker Alyssa P) (Fect Cy D))
; (lives-near (Fect Cy D) (Hacker Alyssa P))
;
; Why does this happen? Is there a way to find a list of people who live near
; each other, in which each pair appears only once? Explain.

; When the runtime executes lives-near, it matches the following pattern in
; the body of the rule
;
; (and (address ?person-1 (?town . ?rest-1))
;      (address ?person-2 (?town . ?rest-2))
;      (not (same ?person-1 ?person-2))))
;
; If scans the assertions for (address ?person-1 (?town . ?rest-1)) and it
; first finds Alyssa. Then it matches the second pattern to eventually find it
; matches Cy and subsequently figure out that he is not the same as Alyssa.
;
; Afterwards, the runtime continues to scan the assertions and it matches the
; first pattern to Cy, subsequently matching the second to Alyssa. That why
; each pair appears twice.
;
; We can find a list in which each pair is listed once if we resort to some
; trickery. We can order the people in some way and assert that the pair has
; to be ordered. A good way to do that is order people alphabetically. Here is
; the new rule:

(add-to-data-base!
  '((rule (ordered-neighbour-pair ?person-1 ?person-2)
          (and (lives-near ?person-1 ?person-2)
               (lisp-value (lambda (person-1 person-2)
                             (string<? (string-join (map symbol->string person-1) " ")
                                       (string-join (map symbol->string person-2) " ")))
                           ?person-1
                           ?person-2)))))
