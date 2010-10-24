OperatorTable addAssignOperator(":", "atPutNumber")

curlyBrackets := method(
    r := Map clone
    call message arguments foreach(arg,
        r doMessage(arg)
    )
    r
)

Map atPutNumber := method(
    self atPut(
        call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
        call evalArgAt(1)
    )
)

phoneNumbers := doString("{\"Bob Smith\": \"5195551212\", \"Mary Walsh\": \"4162223434\"}")
phoneNumbers keys println
phoneNumbers values println
