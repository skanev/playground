; SICP exercise 4.58
;
; Define a rule that says that a person is a "big shot" in a division if the
; person works in the division but does not have a supervisor who works in the
; division.

(add-to-data-base!
  '((rule (big-shot ?person)
          (and (job ?person (?division . ?person-title))
               (not (and (supervisor ?person ?supervisor)
                         (job ?supervisor (?division . ?supervisor-title))))))))
