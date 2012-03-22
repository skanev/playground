; SICP exercise 2.09
;
; The width of an interval is half of the difference between its upper and
; lower bounds. The width is a measure of the uncertainty of the number
; specified by the interval. For some arithmetic operations the width of the
; result of combining two intervals is a function only of the widths of the
; argument intervals, whereas for others the width of the combination is not a
; function of the widths of the argument intervals. Show that the width of the
; sum (or difference) of two intervals is a function only of the widths of the
; intervals being added (or subtracted). Give examples to show that this is not
; true for multiplication or division.

; Let's have two intervals:
;
; i₁ = (l₁, u₁)
; i₂ = (l₂, u₂)
;
; With respective widths:
;
; w₁ = (u₁ - l₁)/2
; w₂ = (u₂ - l₂)/2
;
; When adding i₁ to i₂, we get:
;
; i₃ = i₂ + i₁ = (l₁ + l₂, u₁ + u₂)
; w₃ = (u₁ + u₂)/2 - (l₁ + l₂)/2 = (u₁ - l₁)/2 + (u₂ - l₂)/2 = (w₁ + w₂)/2
;
; When subtracting, we get:
;
; i₃ = i₂ - i₁ = (l₂ - u₁, u₂ - l₁)
; w₃ = (u₂ - l₁)/2 - (l₂ - u₁)/2 = (u₁ + u₂)/2 - (l₁ + l₂)/2 = (w₁ + w₂)/2
;
; Now, let's take a look at multiplication. Let's take two pairs:
;
; (1, 2) x (3, 4) = (3, 8)
; (3, 4) x (5, 6) = (15, 24)
;
; Both pairs have intervals with the same width, but the the resulting
; intervals have different widths.
;
; Let's take a look at division:
;
; (1, 2) ÷ (3, 4) = (1/4, 2/3)
; (3, 4) ÷ (5, 6) = (1/2, 4/5)
;
; Again, the intervals have different widths.
