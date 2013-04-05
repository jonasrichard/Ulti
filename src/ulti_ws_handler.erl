%% Copyright
-module(ulti_ws_handler).
-author("Richard_Jonas").
-behaviour(cowboy_websocket_handler).

%% API
-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp, http}, Req, Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(TransportName, Req, _Opts) ->
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  {reply, {text, <<"The request was: ", Msg/binary>>}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
