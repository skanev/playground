Number oldDivide := Number getSlot("/")
Number / = method(arg,
    if(arg == 0, 0, call target oldDivide(arg))
)

(1 / 2) println
(2 / 3) println
(3 / 0) println
