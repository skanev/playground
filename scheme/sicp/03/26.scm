; SICP exercise 3.26
;
; To search a table as implemented above, one needs to scan the list of
; records. This is basically the unordered list representation of Section
; 2.3.3. For large tables, it may be more efficient to structure the table in
; a different manner. Describe a table implementation where the (key, value)
; records are organized using a binary tree, assuming that keys can be ordered
; in some way (e.g. numerically or alphabetically.) (Compare exercise 2.66 of
; Chapter 2.)

; I am so happy you said "describe".
;
; Anyway, it is fairly simple - we just organize the table in a binary tree
; instead of a list. Whenever we're looking up a key, we're doing a binary
; tree search for the car of the entry of each one and if we find such a node,
; we return the cdr of its entry. This performs in O(log(n)) time, which is
; nice.
;
; The hairy side is insert!. On one hand, we can recreate the tree on every
; insert!. This would keep it balanced and it would perform in O(log(n)), but
; the insertion cost will be huge. On the other hand, modifying the tree is
; not straightforward - if we just find a branch where to add a new element,
; the performance will degrade to O(n), since the tree will not be balanced.
; And if we want to keep it balanced, we need to learn about AVL-trees,
; red-black trees and various other balanced binary trees.
;
; It's not for the weak of heart.
