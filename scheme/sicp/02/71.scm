; SICP exercise 2.71
;
; Suppose we have a Huffman tree for an alphabet of n symbols, and the
; relative frequencies of the symbols are 1, 2, 4,..., 2ⁿ⁻¹. Sketch the tree
; for n = 5; for n = 10. In such a tree (for general n) how many bits are
; required to encode the most frequent symbol? The least frequent symbol?

; Here's the tree for n = 5:
;
;              .
;             / \
;           .    16
;          / \
;        .    8
;       / \
;     .    4
;    / \
;   1   2
;
; The tree for n = 10 is quite similar.
;
; Obviously, the most frequent symbol takes 1 bit and the least frequent
; symbol takes n - 1 (since the depth of the tree is n - 1)
