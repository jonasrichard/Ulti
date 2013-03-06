%% Copyright
-module(echo_listener).
-author("richard").

%% API
-export([start/2, stop/1]).

start(Name, Port) ->
  % Start listening
  case gen_tcp:listen(Port, [binary, {active, false}, {keepalive, true}]) of
    {ok, Socket} ->
      Pid = spawn(fun() -> loop(Socket) end),
      register(Name, Pid),
      {ok, Pid};
    {error, _Reason} = Err ->
      Err
  end
.

stop(Name) ->
  Name ! stop,
  unregister(Name).

loop(ListenSocket) ->
  case gen_tcp:accept(ListenSocket, 100) of
    {ok, Socket} ->
      io:format("Incoming connection"),
      %Pid = spawn(fun() -> echo_receiver:start(Socket) end),
      %gen_tcp:controlling_process(Socket, Pid);
      {ok, Packet} = gen_tcp:recv(Socket, 0),
      io:format("Recv ~p~n", [Packet]),
      gen_tcp:close(Socket);
    {error, timeout} ->
      ok;
    {error, Reason} ->
      io:format("Error during accepting incoming connection ~p~n", [Reason]),
      stop
  end,

  Msg = receive
    stop ->
      gen_tcp:close(ListenSocket),
      stop;
    _ ->
      ok
    after 0 ->
      ok
  end,

  case Msg of
    stop ->
      stop;
    _ ->
      loop(ListenSocket)
  end.
