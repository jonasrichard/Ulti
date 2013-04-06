%% Copyright
-module(ulti_ws_handler).
-author("Richard_Jonas").
%-behaviour(cowboy_websocket_handler).

-record(state, {player_name, room_id, game_pid}).

%% API
-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #state{}}.

websocket_handle({text, Msg}, Req, State) ->
  case parse_command(Msg) of
    [<<"connect">>, Name] ->
      UserName = binary_to_list(Name),
      {reply, {text, <<"connected ", Name/binary>>}, Req, State#state{player_name = UserName}};
    [<<"join">>, Room] ->
      case catch list_to_integer(binary_to_list(Room)) of
        RoomId when is_integer(RoomId) ->
          case gen_server:call(ulti_room_server, {join_room, RoomId, State#state.player_name}) of
            joined ->
              {reply, {text, <<"joined">>}, Req, State#state{room_id = RoomId}};
            room_is_full ->
              {reply, {text, <<"error The room is full">>}, Req, State}
          end;
        _ ->
          {reply, {text, <<"error Invalid room id, must be a number ", Room/binary>>}, Req, State}
      end;
    [<<"put">>, CardMsg] ->
      [Color, Number] = binary:split(CardMsg, <<"_">>),
      Card = {binary_to_atom(Color, latin1), binary_to_atom(Number, latin1)},
      gen_fsm:send_event(State#state.game_pid, {put, Card}),
      {ok, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info(Msg, Req, State) ->
  case Msg of
    {cards, GamePid, Hand} ->
      {reply, {text, io_lib:format("cards ~p", [Hand])}, Req, State#state{game_pid = GamePid}};
    _ ->
      {ok, Req, State}
  end.

websocket_terminate(_Reason, _Req, State) ->
  error_logger:info_msg("User ~p leaving the room ~p~n", [State#state.player_name, State#state.room_id]),
  gen_server:call(ulti_room_server, {leave, self()}),
  ok.

-spec parse_command(Command) -> Words when
  Command  :: binary(),
  Words    :: [binary()].
parse_command(Command) ->
  [Word || Word <- binary:split(Command, <<32>>, [global, trim]), byte_size(Word) > 0].
