%% Copyright
-module(echo_receiver).
-author("richard").

%% API
-export([start/1]).

start(Socket) ->
  io:format("Starting receiver for ~p~n", [Socket]),
  spawn(fun() ->
    io:format("  Setting controlling pid ~p~n", [self()]),
    inet:setopts(Socket, [{packet, 0}, binary, {nodelay, true}, {active, once}]),
    loop(Socket)
  end).

loop(Socket) ->
  receive
    {tcp, Socket, Data} ->
      io:format("Data~n~p~n", [Data]),
      inet:setopts(Socket, [{active, once}]),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Socket is closed~n");
    {tcp_error, Socket, Reason} ->
      io:format("Error during receiving ~p~n", [Reason])
  end.
