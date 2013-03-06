%% Copyright
-module(echo_receiver).
-author("richard").

%% API
-export([start/1]).

start(Socket) ->
  io:format("Starting receiver for ~p~n", [Socket]),
  spawn(fun() -> loop(Socket) end).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  io:format("getopts ~p~n", [inet:getopts(Socket, [active])]),
  receive
    {tcp, Socket, Data} ->
      io:format("Data~n~p~n", [Data]),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Socket is closed~n")
  end.
