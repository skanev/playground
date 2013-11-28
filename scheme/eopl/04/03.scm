; EOPL exercise 4.03

; Write down the specification for a call-exp

;           (value-of exp₁ ρ σ₀) = (val₁, σ₁)
;           (value-of exp₂ ρ σ₁) = (val₂, σ₂)
;           (apply val₁ val₂ σ₂) = (val₃, σ₃)
;   -------------------------------------------------
;   (value-of (call-exp exp₁ exp₂) ρ σ₀) = (val₃, σ₃)
