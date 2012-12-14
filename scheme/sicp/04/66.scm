; SICP exercise 4.66
;
; Ben has been generalizing the query system to provide statistics about the
; company. For example, to find the total salaries of all the computer
; programmers one will be able to say
;
; (sum ?amount
;      (and (job ?x (computer programmer))
;           (salary ?x ?amount)))
;
; In general, Ben's new system allows expressions of the form
;
; (accumulation-function <variable>
;                        <query pattern>)
;
; where accumulation-function can be things like sum, average, or maximum. Ben
; reasons that it should be a cinch to implement this. He will simnply feed
; the query pattern to qeval. This will produce a stream of frames. He will
; then pass this stream through a mapping function that extracts the value of
; the designated variable from each frame in the stream and feed the resulting
; stream of values to the accumulation function. Just as Ben completes the
; implementation and is about to try it out, Cy walks by, still puzzling over
; the wheel query result in exercise 4.65. When Cy shows Ben the system's
; response, Ben groans, "Oh, no, my simple accumulation scheme won't work!".
;
; What has Ben just realized? Outline a method he can use to salvage this
; situation.

; He has realizes that a result may be matched multiple times due to how a
; rule is written. For example, trying to sum the salaries of the wheels will
; result in Oliver Warbucks' salary being counted four times (see exercise
; 4.65).
;
; This can be salvaged by filtering the stream to remove the duplication. We
; can instantiate the query with each frame and check it against previous
; instances. If we encounted a query for a second time, we don't pass it to
; the aggregation function.
