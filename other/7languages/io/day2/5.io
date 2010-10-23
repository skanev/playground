Matrix := Object clone do(
    dim := method(x, y,
        self items := list() setSize(y) map(list() setSize(x))
        self
    )
    set := method(x, y, value,
        items at(y) atPut(x, value)
    )
    get := method(x, y,
        items at(y) at(x)
    )
)

m := Matrix clone dim(2, 4)
m set(0, 0, "top left")
m set(1, 3, "bottom right")
m get(0, 0) println
m get(1, 3) println
