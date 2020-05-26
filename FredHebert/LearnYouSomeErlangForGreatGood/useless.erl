-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).
-compile([debug_info]).
-author("An Erlang Champ").

add(A, B) ->
    A + B.


hello() ->
    io:format("Hello, world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X, 2).
