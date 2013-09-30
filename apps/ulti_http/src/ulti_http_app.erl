%%%==================================================================
%%% @copyright 2012-2013 Richard Jonas
%%% @author: Richard Jonas <richard.jonas.76@gmail.com>
%%% @doc Brief description of the module.
%%%
%%%==================================================================
-module(ulti_http_app).
-behaviour(application).

%% API exports
-export([
        start/0,
        start/2, stop/1
    ]).

%%%------------------------------------------------------------------
%%% Type definitions
%%%------------------------------------------------------------------

%%%==================================================================
%%% API functions
%%%==================================================================

-spec start() -> term().
%%-------------------------------------------------------------------
%% @doc
%% Start an application
%% @end
%% ------------------------------------------------------------------
start() ->
    ok.

%%%==================================================================
%%% Application callbacks
%%%==================================================================

start(_StartType, _Args) ->
    ulti_http_sup:start_link(),
    
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ulti", ulti_ws_handler, []},
            {"/[...]", cowboy_static, [
                {directory, {priv_dir, ulti_http, [<<"www">>]}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
        ]}
    ]),

    cowboy:start_http(ulti_http, 10, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
    cowboy:stop_listener(ulti_http).

%%%==================================================================
%%% Internal functions
%%%==================================================================

