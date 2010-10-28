valid_queen((Row, Col)) :- between(1, 8, Row), between(1, 8, Col).

valid_board([]).
valid_board([Head | Tail]) :- valid_queen(Head), valid_board(Tail).

rows([], []).
rows([(Row, _) | QueensTail], [Row | RowsTail]) :- rows(QueensTail, RowsTail).

cols([], []).
cols([(_, Col) | QueensTail], [Col | ColsTail]) :- cols(QueensTail, ColsTail).

diags1([], []).
diags1([(Row, Col) | QueensTail], [Diagonal | DiagonalsTail]) :-
  Diagonal is Col - Row,
  diags1(QueensTail, DiagonalsTail).

diags2([], []).
diags2([(Row, Col) | QueensTail], [Diagonal | DiagonalsTail]) :-
  Diagonal is Col + Row,
  diags2(QueensTail, DiagonalsTail).

eight_queens(Board) :-
  length(Board, 8),
  valid_board(Board),

  rows(Board, Rows),
  cols(Board, Cols),
  diags1(Board, Diags1),
  diags2(Board, Diags2),

  is_set(Rows),
  is_set(Cols),
  is_set(Diags1),
  is_set(Diags2).
