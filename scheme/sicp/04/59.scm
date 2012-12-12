; SICP exercise 4.59
;
; Ben Bitdiddle has missed on meeting too many. Fearing that his habit of
; forgetting meetings could cost him his job, Ben decides to do something
; about it. He adds the weekly meetings of the firm to the Microshaft data
; base by asserting the following:
;
; (meeting accounting (Monday 9am))
; (meeting administration (Monday 10am))
; (meeting computer (Wednesday 3pm))
; (meeting administration (Friday 1pm))
;
; Each of the above assertions is for a meeting of an entire division. Ben
; also adds an entry for company-wide meeting that spans all the divisions.
; All of the company's employees attend this meeting.
;
; (meeting whole-company (Wednesday 4pm))
;
; a. On Friday morning, Ben wants to query the data base for all the meetings
;    that occur that day. What query should he use?
;
; b. Alyssa P. Hacker is unimpressed. She thinks it would be much more useful
;    to be able to ask for her meetings by specifying her name. So she designs
;    a rule that says a person's mettings include all whole-company meetings
;    plus all meetings of that person's division. Fill in the body of Alyssa's
;    rule.
;
;    (rule (meeting-time ?person ?day-and-time)
;          <rule-body>)
;
; c. Alyssa arrives at work on Wednesday morning and wonders what meetings she
; has to attend that day. Having defined the above rule, what query should she
; make to find this out?

(add-to-data-base!
  '((meeting accounting (Monday 9am))
    (meeting administration (Monday 10am))
    (meeting computer (Wednesday 3pm))
    (meeting administration (Friday 1pm))
    (meeting whole-company (Wednesday 4pm))

    (rule (meeting-time ?person ?day-and-time)
          (or (and (job ?person (?division . ?title))
                   (meeting ?division ?day-and-time))
              (meeting whole-company ?day-and-time)))))

(define bens-query '(meeting ?division (Friday ?time)))
(define alyssas-query '(meeting-time (Hacker Alyssa P) (Wednesday ?time)))
