-module(ulti_game_app).
-author("richard").
-behaviour(application).

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{'_', [
                    {"/echo", ulti_http_handler, []},
                    {"/ws", ulti_ws_handler, []}
                ]}]),
    {ok, _} = cowboy:start_http(ulti_http, 10, [{port, 8080}],
                [{env, [{dispatch, Dispatch}]}]),
  
    ulti_http_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(ulti_http).

