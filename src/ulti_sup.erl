%% Copyright
-module(ulti_sup).
-author("richard").
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  {ok, {{one_for_one, 10, 10}, [
    {ulti_room_server, {ulti_room_server, start_link, []}, permanent, 5000, worker, [ulti_room_server]}
  ]}}.
