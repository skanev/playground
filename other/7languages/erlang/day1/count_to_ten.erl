-module(count_to_ten).
-export([count_to_ten/0]).

count_to_ten() -> count_to(1, 10).

count_to(X, X) -> io:format("~p.~nDone!~n", [X]);
count_to(A, B) -> io:format("~p...~n", [A]), count_to(A + 1, B).

count(X) -> io:format("~p...~n", [X]).
