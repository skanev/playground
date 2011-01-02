% Seven Languages in Seven Weeks, Erlang, day 1, exercise 1:
%
%   Write a function that uses recursion to return the number of
%   words in a string.
%
% This is an awful exercise, given the language features introduced so far
% (pattern matching, lists, function definition). Thus, the solution is a
% hand-rolled state machine that counts transitions from space to character.
-module(word_count).
-export([word_count/1]).

word_count(String) -> word_count(String, space, 0).

word_count([32 | T], space, Count) -> word_count(T, space, Count);
word_count([ _ | T], space, Count) -> word_count(T, character, Count + 1);
word_count([32 | T], character, Count) -> word_count(T, space, Count);
word_count([ _ | T], character, Count) -> word_count(T, character, Count);
word_count([], _, Count) -> Count.
