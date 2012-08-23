; SICP exercise 2.76
;
; As a large system with generic operations evolves, new types of data objects
; or new operations may be needed. For each of the three strategies - generic
; operations with explicit dispatch, data-directed style and
; message-passing-style - describe the changes that must be made to a system
; in order to add new types or new operations. Which organization would be
; most appropriate for a system in which new types must often be added? Which
; would be most appropriate for a system in which new operations must often be
; added?

; This is very similar to Uncle Bob's dichotomy about structures vs. objects.
;
; With generic operations with explicit dispatch, we need to modify every
; existing procedure when we add a new type. Adding a new operation is
; simpler, since we just need to add one procedure.
;
; With message-passing-style, we need to modify all existing types when we add
; a new operation, but we can add a new type without additively.
;
; Data-directed style is a bit more complicated. The structure in the examples
; we've seen so far implies that it is similar to message-passing-style - i.e.
; we can add a new type easily, but adding operations requires modifying the
; existing modules. But this is not true - we can write a module that adds a
; new operation as well, although it needs to know about the types that exist
; so far. To be fair, the packages we've seen so far have been centered around
; a type, but we can organize them around operations too. That way, it is not
; a matter of possibility, but consistency - we can add either new operations
; and new types, but if we have type-centered packages, creating one that
; defines a new operation would be inconsistent. It becomes more complex when
; the packages that are installed need to know about each other - i.e. package
; A introduces an operation and package B introduces a type that implements
; the operation from package A. In all cases, this is the most flexible
; solution.
;
; In object-oriented lingo, generic operations with explicit dispatch is
; similar to structures, message-passing-style is similar to objects and
; data-directed style is quite similar to a sparse Visitor design pattern.
;
; As for the questions, generic operations with explicit dispatch are more
; appropriate for adding a new operation, while message-passing style is more
; appropriate for adding a new type. Data-directed style is less optimal than
; either, but enables both in an additive way.
