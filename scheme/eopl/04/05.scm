; EOPL exercise 4.05
;
; Write down the specification for list (exercise 3.10).

;              (value-of exp₁ ρ σ₀) = (val₁, σ₁)
;              (value-of exp₂ ρ σ₁) = (val₂, σ₂)
;                                  ...
;          (value-of expᵢ ρ σ{i-1}) = (valᵢ, σᵢ)
;   ──────────────────────────────────────────────────────────────────────
;   (value-of (list exp₁ exp₂ ... expᵢ) ρ σ₀) = ([val₁ val₂ ... valᵢ], σᵢ)
