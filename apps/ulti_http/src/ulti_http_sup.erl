-module(ulti_http_sup).
-author("richard").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    {ok, {{one_for_one, 10, 10}, [
        child(ulti_room_server, worker)
    ]}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

child(Module, Type) ->
    {Module,
        {Module, start_link, []},
        permanent,
        5000,
        Type,
        [Module]}.


