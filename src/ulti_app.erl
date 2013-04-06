%% Copyright
-module(ulti_app).
-author("richard").
-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([{'_', [
    {"/echo", ulti_http_handler, []},
    {"/ws", ulti_ws_handler, []}
  ]}]),
  {ok, _} = cowboy:start_http(ulti_http, 10, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
  ulti_sup:start_link().

stop(_State) ->
  cowboy:stop_listener(ulti_http).
