%% Copyright
-module(prod_server).
-author("richard").

-behaviour(gen_server).

%% API
-export([
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  start_link/1]).

-record(state, {number}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Number) ->
  {ok, #state{number = Number}}.

handle_call(Request, From, State) ->
  case Request of
    R when is_number(R) ->
      {reply, R * State#state.number, State};
    _ ->
      {stop, "No number", {error, no_number}, State}
  end.

handle_cast(Request, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  normal.

