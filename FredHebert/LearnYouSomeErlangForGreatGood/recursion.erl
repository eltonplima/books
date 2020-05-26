%%%-------------------------------------------------------------------
%%% @author eltonplima
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. mai 2020 22:28
%%%-------------------------------------------------------------------
-module(recursion).
-compile([debug_info]).
-author("eltonplima").

%% API
-export([
  fac/1,
  fac_tail/1,
  len/1,
  tail_len/1,
  duplicate/2,
  tail_duplicate/2,
  reverse/1,
  tail_reverse/1,
  sublist/2,
  tail_sublist/2,
  zip/2,
  lenient_zip/2,
  tail_zip/2,
  tail_lenient_zip/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Factorial
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fac(0) -> 1;
fac(N) -> N * fac(N - 1).

fac_tail(N) -> fac_tail(N, 1).

fac_tail(0, Acc) -> Acc;
fac_tail(N, Acc) when N > 0 -> fac_tail(N - 1, N * Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Length of a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

len([]) -> 0;
len([_]) -> 1;
len([_ | R]) -> 1 + len(R).

tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | T], Acc) -> tail_len(T, Acc + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Duplicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

duplicate(0, _) -> [];
duplicate(N, Term) -> [Term | duplicate(N - 1, Term)].

tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).
tail_duplicate(0, _, List) -> List;
tail_duplicate(N, Term, List) -> tail_duplicate(N - 1, Term, [Term | List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reverse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

tail_reverse(List) -> tail_reverse(List, []).
tail_reverse([], Acc) -> Acc;
tail_reverse([H | T], Acc) -> tail_reverse(T, [H | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sublist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sublist([], _) -> [];
sublist(_, 0) -> [];
sublist([H | T], N) -> [H | sublist(T, N - 1)].

tail_sublist(List, N) -> reverse(tail_sublist(List, N, [])).

tail_sublist([], _, Sublist) -> Sublist;
tail_sublist(_, 0, Sublist) -> Sublist;
tail_sublist([H | T], N, Sublist) -> tail_sublist(T, N - 1, [H | Sublist]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sublist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% zip([1,2,3],[a,b,c]) = [{1,a}, {2,b}, {3,c}]
zip([], []) -> [];
zip([X|Xs], [Y|Ys]) -> [{X, Y}|zip(Xs, Ys)].

% zip([1,2,3,4],[a,b,c]) = [{1,a}, {2,b}, {3,c}]
% zip([1,2,3],[a,b,c,d]) = [{1,a}, {2,b}, {3,c}]
lenient_zip(_, []) -> [];
lenient_zip([], _) -> [];
lenient_zip([X|Xs], [Y|Ys]) -> [{X, Y}|lenient_zip(Xs, Ys)].

tail_zip(X, Y) -> reverse(tail_zip(X, Y, [])).
tail_zip([], [], ZipList) -> ZipList;
tail_zip([X|Xs], [Y|Ys], ZipList) -> tail_zip(Xs, Ys, [{X, Y}|ZipList]).

tail_lenient_zip(X, Y) -> reverse(tail_lenient_zip(X, Y, [])).
tail_lenient_zip([], _, ZipList) -> ZipList;
tail_lenient_zip(_, [], ZipList) -> ZipList;
tail_lenient_zip([X|Xs], [Y|Ys], ZipList) -> tail_lenient_zip(Xs, Ys, [{X, Y}|ZipList]).
