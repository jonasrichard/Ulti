%% Copyright
-module(ulti_room_server).
-author("richard").
-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0, join/2, leave/1, get_players/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  P = whereis(?MODULE),
  unregister(?MODULE),
  P ! stop.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec join(number(), string()) -> any().
join(RoomId, UserName) ->
  gen_server:call(?MODULE, {join_room, RoomId, UserName}).

-spec leave(number()) -> any().
leave(RoomId) ->
  gen_server:call(?MODULE, {leave_room, RoomId}).

-spec get_players(number()) -> [{UserName, Pid}] when
  UserName  :: string(),
  Pid       :: pid().
get_players(RoomId) ->
  gen_server:call(?MODULE, {get_players, RoomId}).

init(_Args) ->
  ets:new(room, [public, ordered_set, named_table]),
  {ok, undefined}.

handle_call({join_room, RoomId, UserName}, {Pid, _Tag}, State) ->
  error_logger:info_msg("User ~p is joining to room ~p~n", [UserName, RoomId]),
  case ets:lookup(room, RoomId) of
    [] ->
      Users = [{UserName, Pid}],
      ets:insert(room, {RoomId, Users}),
      notify_room_players(Users),
      {reply, joined, State};
    [{_, Users}] when length(Users) < 3 ->
      NewUsers = Users ++ [{UserName, Pid}],
      ets:insert(room, {RoomId, NewUsers}),
      notify_room_players(NewUsers),
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
handle_call({get_players, RoomId}, _From, State) ->
  case ets:lookup(room, RoomId) of
    [] ->
      {reply, [], State};
    [{_, Users}] ->
      {reply, Users, State}
  end;
handle_call(_, _, State) ->
  {noreply, State}.

handle_info(stop, State) ->
  {stop, stopped, State};
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

notify_room_players(Users) ->
  Seq = lists:seq(1, length(Users)),
  Names = [Name || {Name, _Pid} <- Users],
  Room = [P || P <- lists:zip(Seq, Names)],

  [Pid ! {room, Room} || {_Name, Pid} <- Users].
