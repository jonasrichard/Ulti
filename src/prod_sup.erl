%% Copyright
-module(prod_sup).
-author("richard").

-behaviour(supervisor).

%% API
-export([
  start_link/0, start_child/1,
  init/1]).

start_link() ->
  supervisor:start_link({local, product_sup}, ?MODULE, []).

start_child(Number) ->
  supervisor:start_child(product_sup, [Number]).

init([]) ->
  {ok,
    {
      {simple_one_for_one, 0, 1},
      [{prod_server, {prod_server, start_link, []}, temporary, brutal_kill, worker, [prod_server]}]
    }
  }.
