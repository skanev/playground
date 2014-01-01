; EOPL exercise 4.06
;
; Modify the rule given above so that setref-exp returns the value of the
; right-hand side.

;             (value-of exp₁ ρ σ₀) = (l, σ₁)
;             (value-of exp₂ ρ σ₁) = (val, σ₂)
;   ─────────────────────────────────────────────────────────
;   (value-of (setref-exp exp₁ exp₂) ρ σ₀) = (val, [l=val]σ₂)
