; EOPL exercise 2.02
;
; Analyze each of these proposed representations critically. To what extent do
; they succeed or fail in satisfying the specification of the datatype?

; All of the representations satisfy the specification fully.
;
; 1. Unary representation
;
; This is extremely memory hungry. It is also equivalent to using bignums with
; base 1. It does not allow us to create very large numbers, but all the
; operations are performed in constant time. If the memory is limitless, this
; representation will allow us to create arbitrary large numbers.
;
; 2. Scheme number representation:
;
; Depending on whether the Scheme numbers are seamlessly converted to bignums
; (which they usually are), this representation might not allow us to
;
; 3. Bignum representation:
;
; This representation is more memory-efficient than the first, but operations
; are not in constant time. That is, calling successor on a n-sized list of
; base - 1 will take O(n) time. It is, of course, the most sensible way to
; implement numbers, provided that we implement the arithmetic operations by
; depending on the representation, instead of the four observers.
