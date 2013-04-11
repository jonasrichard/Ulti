%% Copyright
-module(ulti_room_SUITE).
-author("Richard_Jonas").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_registered/1, test_simple_join/1]).

all() ->
  [test_registered, test_simple_join].

init_per_suite(Config) ->
  Res = ulti_room_server:start(),
  ct:log("Ulti room server started: ~p", [Res]),
  Config.

end_per_suite(_Config) ->
  ulti_room_server:stop().

test_registered(_Config) ->
  SrvPid = whereis(ulti_room_server),
  ct:log("Server pid is ~p", [SrvPid]),
  ?assert(is_pid(SrvPid)),
  ?assert(is_process_alive(SrvPid)).

test_simple_join(_Config) ->
  R = ulti_room_server:join(3, "Smith"),
  ?assertEqual(joined, R),
  ulti_room_server:join(3, "John"),
  ulti_room_server:join(3, "Jack"),
  Users = ulti_room_server:get_players(3),
  [?assert(lists:member(Name, ["Smith", "John", "Jack"])) || {Name, _Pid} <- Users].
