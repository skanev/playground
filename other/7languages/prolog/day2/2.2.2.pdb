smallest(Min, [Min]).
smallest(Min, [A, B | Tail]) :- A =< B, smallest(Min, [A | Tail]).
smallest(Min, [A, B | Tail]) :- A >= B, smallest(Min, [B | Tail]).
