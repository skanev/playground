; EOPL exercise B.01
;
; The following grammar for ordinary arithmetic expressions builds in the
; usual precendence in the usual rules for arithmetic operations:
;
;   Arith-expr        ::= Arith-term {Additive-op Arith-term}*
;   Arith-term        ::= Arith-factor {Multiplicative-op Arith-factor}*
;   Arith-factor      ::= Number
;                     ::= (Arith-expr)
;   Additive-op       ::= + | -
;   Multiplicative-op ::= * | /
;
; This grammar says that every arithmetic expression is the sum of a non-empty
; sequence of terms; every term is the product of a non-empty sequence of
; factors; and every factor is either a constant or a parenthesized
; expression.
;
; Write a lexical specification and a grammar in SLLGEN that will scan and
; parse strings according to this grammar. Verify that this grammar handles
; precendence correctly, so that, for example 3+2*66-5 gets grouped correctly,
; as 3 + (2 × 66) - 5.

(define scanner-spec
  '((white-sp (whitespace) skip)
    (additive-op ((or "+" "-")) symbol)
    (multiplicative-op ((or "*" "/")) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression
      (term (arbno additive-op term))
      op)
    (term
      (factor (arbno multiplicative-op factor))
      op)
    (factor
      (number)
      number)
    (factor
      ("(" expression ")")
      factor)))

(define-datatype ast ast?
  (op
    (first-operand ast?)
    (operators (list-of symbol?))
    (rest-operands (list-of ast?)))
  (factor
    (value ast?))
  (number
    (value integer?)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))
