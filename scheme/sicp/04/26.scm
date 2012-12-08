; SICP exercise 4.26
;
; Ben Bitdidle and Alyssa P. Hacker disagree over the importance of lazy
; evaluation for implementing things such as unless. Ben points out that it's
; possible to implement unless in applicative order as a special form. Alyssa
; counters that, if one did that, unless would be merely syntax, not a
; procedure that could be used in conjunction with higher-order procedures.
; Fill in the details on both sides of the argument. Show how to implement
; unless as a derived expression (like cond or let), and give an example of a
; situation where it might be useful to have unless as a procedure, rather
; than as a special form.

; "Merely syntax". Heh.
;
; On Ben's side, we can add that there is nothing wrong with having unless
; implemented as a special form. I can't see a case where one would want to
; pass unless as an argument. Granted, it requires a change to the interpreter
; or the introduction of macro mechanisms. Converting to lazy evaluation
; raises some hairy questions when side effects are involved (as we shall see
; in the upcomming exercises) and carries a certain performance overhead.
;
; On Alyssa's side, it would actually be cool to pass unless as a function.
; While unless is not the best example, lazy evaluation would be useful for
; streams - the cons-stream form from the previous chapter can be replaced
; with cons. Furthermore, we won't need a macro facility.
;
; Here is how unless can be implemented as a special form:

(define (unless->combination condition usual-value exceptional-value)
  (list 'if condition exceptional-value usual-value))

; It of course, needs to be installed in eval.
;
; As for an example of a situation where it might be useful to have unless as
; a procedure, I'm going to point out something similar I already mentioned -
; cons-stream. Lazy evaluation would remove the necessity to add a special
; form and we can just use cons instead.
