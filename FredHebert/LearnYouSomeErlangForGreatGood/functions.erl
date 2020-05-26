%%%-------------------------------------------------------------------
%%% @author eltonplima
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. mai 2020 13:20
%%%-------------------------------------------------------------------
-module(functions).
-author("eltonplima").

%% API
%%-export([]).
-compile([export_all]).

head([H|_]) -> H.
second([_, S|_]) -> S.

same(X, X) -> true;
same(_, _) -> false.

valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
  io:format("The Date tuple (~p) says today is: ~p/~p/~p, ~n", [Date,Y,M,D]),
  io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
  io:format("Stop feeding me wrong data!~n").
