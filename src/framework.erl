%% Copyright
-module(framework).
-author("richard").

%% API
-export([start/0, send/1]).

start() ->
  echo_listener:start(echo, 8888),
  {ok, S} = gen_tcp:connect("localhost", 8888, []),
  S.

send(S) ->
  gen_tcp:send(S, "Test string").
