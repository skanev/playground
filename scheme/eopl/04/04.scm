; EOPL exercise 4.04
;
; Write down the specification for a begin expression.
;
;   Expression ::= begin Expression {; Expression}* end
;
; A begin expression may contain one or more subexpressions separated by
; semicolons. These are evaluated in order and the value of the last is
; returned.

;              (value-of exp₁ ρ σ₀) = (val₁, σ₁)
;              (value-of exp₂ ρ σ₁) = (val₂, σ₂)
;                                  ...
;          (value-of expᵢ ρ σ{i-1}) = (valᵢ, σᵢ)
;   ───────────────────────────────────────────────────────
;   (value-of (begin exp₁ exp₂ ... expᵢ) ρ σ₀) = (valᵢ, σᵢ)
