Builder := Object clone
Builder spacing := 0

Builder forward := method(
    printWithSpacing("<", call message name, ">")
    increaseSpacing
    call message arguments foreach(arg,
        content := self doMessage(arg)
        if(content type == "Sequence", printWithSpacing(content))
    )
    decreaseSpacing
    printWithSpacing("</", call message name, ">")
)
Builder increaseSpacing := method(self spacing := spacing + 1)
Builder decreaseSpacing := method(self spacing := spacing - 1)
Builder printWithSpacing := method(
    spacing repeat("    " print)
    call delegateToMethod(self, "writeln")
)

Builder ul(
    li("Io"),
    li("Lua"),
    li("JavaScript")
)
