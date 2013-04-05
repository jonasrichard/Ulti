%% Copyright
-module(ulti_http_handler).
-author("richard").
-behaviour(cowboy_http_handler).

%% API
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {Echo, Req3} = cowboy_req:qs_val(<<"echo">>, Req2),
  {ok, Req4} = cowboy_req:reply(200, [], "Echo: " ++ Echo, Req3),
  {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
  ok.
