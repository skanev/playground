OperatorTable addAssignOperator(":", "makeHashPair")

CoolBuilder := Object clone do(
    spacing := 0
    forward := method(
        tagName := call message name
        items := call message arguments

        if(items at(0) ?name == "curlyBrackets",
                printWithSpacing("<", tagName, " ", AttributesPrinter makeAttributesString(items removeFirst), ">")
            ,
                printWithSpacing("<", tagName, ">")
        )
        printContents(items)
        printWithSpacing("</", tagName, ">")
    )
    printContents := method(contents,
        self spacing = spacing + 1
        contents foreach(item,
            content := self doMessage(item)
            if(content type == "Sequence", printWithSpacing(content))
        )
        self spacing = spacing - 1
    )
    printWithSpacing := method(
        spacing repeat("    " print)
        call delegateToMethod(self, "writeln")
    )
)

AttributesPrinter := Map clone do(
    makeAttributesString := method(msg,
        printer := AttributesPrinter clone
        printer doMessage(msg)
        printer map(key, value, key .. "=\"" .. value .. "\"") join(" ")
    )
    curlyBrackets := method(
        call message arguments foreach(arg, doMessage(arg))
    )
    makeHashPair := method(
        self atPut(
            call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
            call evalArgAt(1)
        )
    )
)

doFile("3.xml.io")
