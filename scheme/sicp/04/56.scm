; SICP exercise 4.56
;
; Formulate compound queries that retrieve the following information:
;
; a. the names of all people who are supervised by Ben Bitdiddle, together
;    with their addresses
; b. all people whose salary is less than Ben Bitdiddle's, together with their
;    salary and Ben Bitdiddle's salary
; c. all people who are supervised by someone who is not in the computer
;    division, together with the supervisor's name and job

(define query-a '(and (address ?person ?address)
                      (supervisor ?person (Bitdiddle Ben))))
(define query-b '(and (salary ?person ?person-salary)
                      (salary (Bitdiddle Ben) ?bens-salary)
                      (lisp-value < ?person-salary ?bens-salary)))
(define query-c '(and (supervisor ?person ?supervisor)
                      (not (job ?supervisor (computer . ?title)))))
