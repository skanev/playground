smallest(Number, List) :- member(Number, List), less_or_equal_than(Number, List).

member(Number, [Number|_]).
member(Number, [_|Tail]) :- member(Number, Tail).

less_or_equal_than(Number, []).
less_or_equal_than(Number, [Head|Tail]) :- Number =< Head, less_or_equal_than(Number, Tail).
