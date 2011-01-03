-module(pseudo_dict).
-export([value_in/2]).

value_in([{Key, Value} | _], Key) -> Value;
value_in([_ | Tail], Key) -> value_in(Tail, Key);
value_in([], _) -> undefined.
