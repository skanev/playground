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
    transpose := method(
        n := items at(0) size
        transposedItems := list
        for(i, 0, n - 1, transposedItems append(items map(at(i))))
        newMatrix := Matrix clone
        newMatrix items := transposedItems
        newMatrix
    )
)

m := Matrix clone dim(2, 4)
m set(0, 3, "top right")
m set(1, 0, "bottom left")

t := m transpose
(t get(3, 0) == m get(0, 3)) println
(t get(0, 1) == m get(1, 0)) println
