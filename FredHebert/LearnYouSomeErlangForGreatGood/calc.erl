%%%-------------------------------------------------------------------
%%% @author eltonplima
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. mai 2020 19:56
%%%-------------------------------------------------------------------
-module(calc).
-author("eltonplima").

%% API
-export([rpn/1,rpn_test/0]).

rpn(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
  Res.

read(N) ->
  case string:to_float(N) of
    {error, no_float} -> list_to_integer(N);
    {F, _} -> F
  end.

rpn("+", [N1, N2|Stack]) -> [N2+N1|Stack];
rpn("-", [N1, N2|Stack]) -> [N2-N1|Stack];
rpn("*", [N1, N2|Stack]) -> [N2*N1|Stack];
rpn("/", [N1, N2|Stack]) -> [N2/N1|Stack];
rpn("^", [N1, N2|Stack]) -> [math:pow(N2,N1)|Stack];
rpn("ln", [N1|Stack]) -> [math:log(N1)|Stack];
rpn("log10", [N1|Stack]) -> [math:log10(N1)|Stack];
rpn("sum", Stack) -> [lists:sum(Stack)];
rpn(X, Stack) -> [read(X)|Stack].

rpn_test() ->
  5 = rpn("2 3 +"),
  87 = rpn("90 3 -"),
  -4 = rpn("10 4 3 + 2 * -"),
  -2.0 = rpn("10 4 3 + 2 * - 2 /"),
  ok = try
         rpn("90 34 12 33 55 66 + * - +")
       catch
         error:{badmatch,[_|_]} -> ok
       end,
  4037 = rpn("90 34 12 33 55 66 + * - + -"),
  8.0 = rpn("2 3 ^"),
  true = math:sqrt(2) == rpn("2 0.5 ^"),
  true = math:log(2.7) == rpn("2.7 ln"),
  true = math:log10(2.7) == rpn("2.7 log10"),
  50 = rpn("10 10 10 20 sum"),
  ok.
