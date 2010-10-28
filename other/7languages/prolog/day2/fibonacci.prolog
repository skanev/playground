fib(0, 1).
fib(1, 1).
fib(X, Y) :- S1 is X - 1, S2 is X - 2, fib(S1, F1), fib(S2, F2), Y is F1 + F2.
