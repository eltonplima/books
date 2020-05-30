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
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      case valid_datetime(TimeOut) of
        true ->
          EventPid = event:start_link(Name, TimeOut),
          NewEvents = orddict:store(Name,
            #event{name=Name, description=Description, pid=EventPid, timeout=TimeOut},
            S#state.events),
          Pid ! {MsgRef, ok},
          loop(S#state{events=NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_timeout}},
          loop(S)
      end;
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

valid_datetime({Date, Time}) ->
  try
    calendar:valid_date(Date) andalso valid_time(Time)
  catch
    error:function_clause ->
       %% Not in {{D,M,Y}{H,Min,S}} format
      false
  end;

valid_datetime(_) -> false;

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
  M >= 0, M < 60,
  S >= 0, S < 60 -> true;
valid_time(_, _, _) -> false.
