sorted([H | T], B, C) :- inserted(H, B, B1), sorted(T, B1, C).
sorted([], X, X).

inserted(A, [], [A]).
inserted(A, [H | T], [A, H | T]) :- A < H.
inserted(A, [H | T1], [H | T2]) :- H < A, inserted(A, T1, T2).
