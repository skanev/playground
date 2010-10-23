Matrix := Object clone do(
    dim := method(x, y,
        withItems list() setSize(y) map(list() setSize(x))
    )
    withItems := method(items,
        self items := items
        self
    )
    set := method(x, y, value,
        items at(y) atPut(x, value)
    )
    get := method(x, y,
        items at(y) at(x)
    )
    saveTo := method(filename,
        file := File with(filename)
        file remove
        file openForUpdating
        file write(items map(join(",")) join("\n"))
        file close
    )
    readFrom := method(filename,
        file := File with(filename)
        file openForReading
        readItems := file readLines map(split(",") map(asNumber))
        file close
        withItems(readItems)
    )
)

original := Matrix clone withItems(list(list(1, 2), list(3, 4)))
original saveTo("matrix.txt")

reloaded := Matrix clone readFrom("matrix.txt")
reloaded items println
(reloaded items == original items) println

File with("matrix.txt") remove
