%% Copyright
-module(ulti_http).
-author("richard").

%% API
-export([start/0]).

start() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowboy),
  Dispatch = cowboy_router:compile([{'_', [
    {"/echo", ulti_http_handler, []},
    {"/ws", ulti_ws_handler, []}
  ]}]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
          [{env, [{dispatch, Dispatch}]}]).
