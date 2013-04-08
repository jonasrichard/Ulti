%% Copyright
-module(ulti_room_server).
-author("richard").
-behaviour(gen_server).

%% API
-export([start_link/0, join/2, leave/1,
         init/1, handle_call/3, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(RoomId, UserName) ->
  gen_server:call(?MODULE, {join_room, RoomId, UserName}).

leave(RoomId) ->
  gen_server:call(?MODULE, {leave_room, RoomId}).

init(_Args) ->
  ets:new(room, [public, ordered_set, named_table]),
  {ok, undefined}.

handle_call({join_room, RoomId, UserName}, {Pid, _Tag}, State) ->
  error_logger:info_msg("User ~p is joining to room ~p~n", [UserName, RoomId]),
  case ets:lookup(room, RoomId) of
    [] ->
      ets:insert(room, {RoomId, [{UserName, Pid}]}),
      {reply, joined, State};
    [{_, Users}] when length(Users) < 3 ->
      NewUsers = Users ++ [{UserName, Pid}],
      ets:insert(room, {RoomId, NewUsers}),
      if length(NewUsers) == 3 ->
        ulti_play:start_game(NewUsers);
      true ->
        ok
      end,
      {reply, joined, State};
    _ ->
      {reply, room_is_full, State}
  end;
handle_call({leave_room, RoomId}, {Pid, _Tag}, State) ->
  case ets:lookup(room, RoomId) of
    [] ->
      {reply, left_the_room, State};
    [{_, Users}] ->
      ets:insert(room, {RoomId, lists:keydelete(Pid, 2, Users)}),
      {reply, left_the_room, State}
  end;
handle_call(_, _, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
