; SICP exercise 4.55
;
; Give simple queries that retrieve the following information from the data
; base:
;
; a. all people supervised by Ben Bitdiddle
; b. the names and jobs of all people in the accounting division
; c. the names and addresses of all people who live in Slumerville

(define query-a '(supervisor ?person (Bitdiddle Ben)))
(define query-b '(job ?person (accounting . ?rest)))
(define query-c '(address ?person (Slumerville . ?rest)))
