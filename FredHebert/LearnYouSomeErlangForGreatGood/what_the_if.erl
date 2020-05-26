%%%-------------------------------------------------------------------
%%% @author eltonplima
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. mai 2020 17:49
%%%-------------------------------------------------------------------
-module(what_the_if).
-author("eltonplima").

%% API
-export([heh_fine/0, oh_god/1, help_me/1]).

heh_fine() ->
  if 1 =:= 1 -> works end,
  if 1 =:= 2 -> works end,
  if 1 =:= 2, 1 =:= 1 -> fails end.

oh_god(N) ->
  if N =:= 2 -> might_succeed;
    true -> always_does
  end.

help_me(Animal) ->
  Talk = if Animal == cat -> "meow";
           Animal == beef -> "mooo";
           Animal == dog -> "bark";
           Animal == tree -> "bark";
           true -> "fafafafafa"
         end,
  {Animal, "says " ++ Talk ++ "!"}.
