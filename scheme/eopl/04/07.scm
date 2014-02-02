; EOPL exercise 4.07
;
; Modify the rule given above so that setref-exp returns the old contents of
; the location.

;             (value-of exp₁ ρ σ₀) = (l, σ₁)
;             (value-of exp₂ ρ σ₁) = (val, σ₂)
;   ──────────────────────────────────────────────────────────
;   (value-of (setref-exp exp₁ exp₂) ρ σ₀) = (σ₁(l), [l=val]σ₂)
