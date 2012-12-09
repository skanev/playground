; SICP exercise 4.32
;
; Give some examples that illustrate the difference between the streams of
; chapter 3 and the "lazier" lazy lists described in this section. How can you
; take advantage of this extra laziness?

; Well, one of them, obviously, is implementing solve without the necessity
; for delays. This can be generalized for all infinite data structures -
; instead of having to introduce a special form in the language, we can have a
; constructor that lazily evaluates its parameters. A favourite of mine is
; implementing map via accumulate, which cannot happen otherwise. It is not
; the most practical, but it is cool nontheless.
