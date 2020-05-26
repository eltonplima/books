%%%-------------------------------------------------------------------
%%% @author eltonplima
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. mai 2020 18:09
%%%-------------------------------------------------------------------
-module(cases).
-author("eltonplima").

%% API
-export([insert/2]).

insert(X, []) -> [X];
insert(X, Set) ->
  case lists:member(X, Set) of
    true -> Set;
    false -> [X|Set]
  end.

beach(Temperature) ->
  case Temperature of
    {celsius, N} when N >= 20, N =< 45 -> 'favorable';
    {kelvin, N} when N >= 293, N =< 318 -> 'scientifically favorable';
    {fahrenheit, N} when N >= 68, N =< 113 -> 'favorable in the US';
    _ -> 'avoid beach'
  end.
