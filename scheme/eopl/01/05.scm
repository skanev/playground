; EOPL exercise 1.05
;
; Prove that if e ∈ LcExp, then there are the same number of left and right
; parenthesis in e.

; The lambda expression grammar is:
;
; LcExp ::= Identifier
;       ::= (lambda (Identifier) LcExp)
;       ::= (LcExp LcExp)
;
; I don't think I need induction here, but let's try to put it in.
;
; Proof: The proof is by induction on the size of e, where we take the size of
; e to be the number of productions in the grammar. The induction hypothesis
; IH(k), is that any expression with number of productions ≤ k has the same
; number of left and right parenthesis.
;
; 1. There is only one expression with 1 production and this is Identifier.
;    All other have a LcExp non-terminal, thus requiring more productions. By
;    definition, identifiers don't include parenthesis, so IH(1) holds.
; 2. Let k be an integer such that IH(k) holds. We will show that IH(k + 1)
;    holds as well. If e requires ≤ k + 1 productions, there are three
;    possibilities in the grammar:
;
;      (a) e can be of the form Identifer. There are no parenthesis, so this
;          holds trivially.
;      (b) e can be of the form (lambda (Identifier) e₁). The number of left
;          parenthesis is 2 + left-parens(e₁). The number of right parenthesis
;          is 2 + right-parens(e₁). Since e requires ≤ k + 1 productions, we
;          can infer that e₁ requires ≤ k productions. From the hypothesis, we
;          know that e₁ has the same number of left and right parenthesis,
;          thus IH(k + 1) holds.
;      (c) e can be of the form (e₁ e₂), where e₁ and e₂ are of the form
;          LcExp. Since e requires ≤ k + 1 productions, both require ≤ k
;          productions and we know that each has the same number of
;          parenthesis. Thus if p₁ = left-parens(e₁) = right-parens(e₁) and
;          p₂ = left-parens(e₂) = right-parens(e₂), then we see that:
;
;          left-parens(e)  = 1 + left-parens(e₁) + left-parens(e₂ )  = 1 + p₁ + p₂
;          right-parens(e) = 1 + right-parens(e₁) + right-parens(e₂) = 1 + p₁ + p₂
;
;          and thus
;
;          left-parens(e) = right-parens(2)
;
;          which proves that IH(k + 1) holds.
;
; This completes the proof of the claim that IH(k + 1) holds and therefore
; completes the induction.∎
