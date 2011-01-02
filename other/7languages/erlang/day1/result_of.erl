-module(result_of).
-export([result_of/1]).

result_of(success) -> io:format("Success.~n");
result_of({error, Message}) -> io:format("Error occurred: ~s.~n", [Message]).
