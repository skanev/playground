; SICP exercise 1.13
;
; Prove that Fib(n) is the closest integer to 𝜙ⁿ/√5, where 𝜙 = (1 + √5)/2.
;
; Hint: Let 𝜓 = (1 - √5)/2. Use induction and the definition of Fibonacci numbers
; (see section 1.2.2) to prove that Fib(n) = (𝜙ⁿ - 𝜓ⁿ)/√5

; Seriously? Anyway...
;
; First, let's establish that |𝜓ⁿ| < ½ for n > 1. It holds, because:
;
; 𝜓 ≈ -0.61803
; 𝜓² ≈ 0.38196
;
; As n grows, it converges to 0.
;
; Next, let's illustrate that 𝜓² = 𝜓 + 1:
;
; 𝜓² = (1 - √5)²/2² = (6 - 2√5)/4 = (3 - √5)/2 = 2/2 + (1 - √5)/2 = 1 + 𝜓
;
; Afterwards let's prove Fib(n) = (𝜙ⁿ - 𝜓ⁿ)/√5, using induction.
;
; Basis. We will show it holds true for n = 0 and n = 1.
;
; (𝜙⁰ - 𝜓⁰)/√5 = (1 - 1)/√5 = 0/√5 = 0 = Fib(0)
; (𝜙 - 𝜓)/√5 = (1 + √5 - 1 + √5)/2√5 = 2√5/2√5 = 1 = Fib(1)
;
; Inductive step. We can assume that the following hold:
;
; (𝜙ⁿ - 𝜓ⁿ)/√5 = Fib(n)
; (𝜙ⁿ⁺¹ - 𝜓ⁿ⁺¹)/√5 = Fib(n + 1)
;
; Let's prove that (𝜙ⁿ⁺² - 𝜓ⁿ⁺²)/√5 = Fib(n + 2).
;
; Fib(n + 2) = Fib(n + 1) + Fib(n) = (𝜙ⁿ⁺¹ - 𝜓ⁿ⁺¹)/√5 + (𝜙ⁿ - 𝜓ⁿ)/√5 =
;            = (𝜙ⁿ⁺¹ + 𝜙ⁿ - 𝜓ⁿ⁺¹ - 𝜓ⁿ)/√5 = (𝜙ⁿ(𝜙 + 1) - 𝜓ⁿ(𝜓 + 1))/√5 =
;            = (𝜙ⁿ⁺² - 𝜓ⁿ⁺²)/√5
;
; The only thing left is to relate (𝜙ⁿ - 𝜓ⁿ)/√5 to the statement we are
; proving - Fib(n) is the closest integer to 𝜙ⁿ/√5.
;
; Fib(n) - 𝜙ⁿ/√5 = 𝜓ⁿ/√5
;
; We already know that 𝜓ⁿ/√5 is less than ½, which makes Fib(n) the closest
; integer to 𝜙ⁿ/√5.
