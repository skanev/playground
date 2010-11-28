val strings = List("Foo", "Bar", "Baz")
val totalSize = (0 /: strings) { _ + _.size }
println(totalSize)
