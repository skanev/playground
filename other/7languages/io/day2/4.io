List myAverage := method(
    isEmpty ifTrue(Exception raise("List has no elements"))
    select(proto != Number) isEmpty ifFalse(Exception raise("List contains non-number elements"))
    sum / size
)

(list(1, 2, 3) myAverage == 2) println
(list(1, 2, 3, 4) myAverage == 2.5) println
try(list() myAverage) error println
try(list(22, 0xF, "Sofia") myAverage) error println
