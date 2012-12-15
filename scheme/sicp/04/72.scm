; SICP exercise 4.72
;
; Why do disjoin and stream-flatmap interleave the streams rather than simply
; append them? Give examples that illustrate why interleaving works better.
; (Hint: Why did we use interleave in section 3.5.3?)

; It is pretty much the same as in last exercise. If the first stream is
; infinite, this gives a chance for elements of the second stream to be
; returned. If we don't interleave, the first stream will possibly not let the
; interpreter report terminating results from the second stream.
;
; However, this still does not make a big difference in my opinion. The
; interpreter gets a chance to to report some results before getting stuck,
; but in the long run, I don't think that makes a meaningful difference.
