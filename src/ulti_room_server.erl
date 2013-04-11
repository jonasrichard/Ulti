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

join(RoomId, UserName) ->
  gen_server:call(?MODULE, {join_room, RoomId, UserName}).

leave(RoomId) ->
  gen_server:call(?MODULE, {leave_room, RoomId}).

get_players(RoomId) ->
  gen_server:call(?MODULE, {get_players, RoomId}).

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
