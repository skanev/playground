; SICP exercise 2.13
;
; Show that under the assumption of small percentage tolerances there is a
; simple formula for the approximate percentage tolerance of the product of two
; intevals in terms of the tolerances of the factors. You may simplify the
; problem by assuming that all numbers are positive.

; Let's assume have the following intervals
;
; i₁ = ((1 - 0.5t₁)x, (1 + 0.5t₁)x)
; i₂ = ((1 - 0.5t₂)y, (1 + 0.5t₂)y)
;
; If we multiply them, we get
;
; i₃ = i₁i₂ = (l, u), where
;
; l = (1 - 0.5t₁)(1 - 0.5t₂)xy = 1 - 0.5t₁ - 0.5t₂ + 0.25t₁t₂
; u = (1 + 0.5t₁)(1 + 0.5t₂)xy = 1 + 0.5t₁ + 0.5t₂ + 0.25t₁t₂
;
; We know that t₁ and t₂ are very small, which means that t₁t₂ is neglectable.
; If we ignore it, we get the interval:
;
; i₃ = ((1 - 0.5(t₁ + t₂))xy, (1 + 0.5(t₁ + t₂))xy)
;
; Thus, the formula is:
;
; t₃ = t₁ + t₂
