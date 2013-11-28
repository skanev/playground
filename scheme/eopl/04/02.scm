; EOPL exercise 4.02

; Write down the specification for a zero?-exp

;   (value-of exp₁ ρ σ₀) = (val₁, σ₁)
;   -------------------------------
;   (value-of (zero?-exp val₁) ρ σ₀)
;     = { ((bool-val #t), σ₁)   if (expval->num val₁) = 0
;       { ((bool-val #f), σ₁)   if (expval->num val₁) ≠ 0
