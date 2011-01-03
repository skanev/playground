-module(tictactoe).
-export([outcome/1]).

outcome([W, W, W, _, _, _, _, _, _]) -> W;
outcome([_, _, _, W, W, W, _, _, _]) -> W;
outcome([_, _, _, _, _, _, W, W, W]) -> W;
outcome([W, _, _, W, _, _, W, _, _]) -> W;
outcome([_, W, _, _, W, _, _, W, _]) -> W;
outcome([_, _, W, _, _, W, _, _, W]) -> W;
outcome([W, _, _, _, W, _, _, _, W]) -> W;
outcome([_, _, W, _, W, _, W, _, _]) -> W;
outcome(Board) ->
    case lists:all(fun(E) -> (E =:= x) or (E == o) end, Board) of
        true -> cat;
        false -> no_winner
    end.
