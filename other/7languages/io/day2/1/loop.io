fib := method(number,
    a := 1
    b := 1
    (number - 2) repeat (
        b = a + b
        a = b - a
    )
    b
)

fib(1) println
fib(2) println
fib(3) println
fib(4) println
fib(5) println
fib(6) println
fib(7) println
fib(8) println
fib(9) println
fib(10) println
