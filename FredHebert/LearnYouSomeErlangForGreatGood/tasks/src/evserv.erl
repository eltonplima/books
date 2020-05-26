%%%-------------------------------------------------------------------
%%% @author eltonplima
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. mai 2020 05:12
%%%-------------------------------------------------------------------
-module(evserv).
-author("eltonplima").

%% API
%%-export([]).
-compile(export_all).
-record(state, {events,  %% list of #event[] records
  clients}).  %% list of Pids
-record(event, {name="", description="", pid, timeout={{1970,1,1},{0,0,0}}}).

loop(S = #state{}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      pass;
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      pass;
    {Pid, MsgRef, {cancel, Name}} ->
      pass;
    {done, Name} ->
      pass;
    shutdown ->
      pass;
    {'DOWN', Ref, process, _Pid, _Reason} ->
      pass;
    code_change ->
      pass;
    Unknown ->
      io:format("Unknown message ~p~n", [Unknown]),
      loop(State)
  end.

init() ->
  loop(#state{events=orddict:new(), clients=orddict:new()}).
