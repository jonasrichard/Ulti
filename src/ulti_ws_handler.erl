%% Copyright
-module(ulti_ws_handler).
-author("Richard_Jonas").

-include("ulti_game.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
  player_name      :: string(),
  room_id          :: integer(),
  player           :: pid()
}).

%% API
-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-export([convert_card_test/0, convert_hand_test/0, parse_card_test/0]).

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
          case ulti_room_server:join(RoomId, State#state.player_name) of
            joined ->
              {ok, Req, State#state{room_id = RoomId}};
            room_is_full ->
              {reply, {text, <<"error The room is full">>}, Req, State}
          end;
        _ ->
          {reply, {text, <<"error Invalid room id, must be a number ", Room/binary>>}, Req, State}
      end;
    [<<"put">>, CardMsg] ->
      Card = parse_card(CardMsg),
      gen_event:notify(State#state.player, {put, Card}),
      {ok, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

%%
%% Client can get the following type of messages
%% - joined 2                     the just joined player will be no. 2 player
%% - room 1, Joe, 2 Jack ...      the number of the player (can be 1, 2, 3)
%% - hand ...                     the cards on the player's hand
%% - start 3                      player 3 will start
%% - other_hand 1 5               player 1 has 5 cards
%% - put 2 tok_also               player 2 put tok also
%% - take 3                       player 3 took the table's cards
%% - result 2                     player 2 won
%%
websocket_info(Msg, Req, State) ->
  case Msg of
    {set_event_handler, EvtPid} ->
      {ok, Req, State#state{player = EvtPid}};
    you_can_put_card ->
      {reply, {text, "you_can_put_card"}, Req, State};
    {joined, No} ->
      {reply, {text, io_lib:format("joined ~w", [No])}, Req, State};
    {hand, Hand} ->
      {reply, {text, io_lib:format("cards ~s", [convert_hand(Hand)])}, Req, State};
    {room, Users} ->
      {reply, {text, io_lib:format("room ~s", [convert_room_players(Users)])}, Req, State};
    {put, No, Card} ->
      {reply, {text, io_lib:format("put ~w ~s", [No, convert_card(Card)])}, Req, State};
    take ->
      {reply, {text, "take"}, Req, State};
    {taker, PlayerName} ->
      {reply, {text, io_lib:format("taker ~w", [PlayerName])}, Req, State};
    _ ->
      {ok, Req, State}
  end.

websocket_terminate(_Reason, _Req, State) ->
  error_logger:info_msg("User ~p leaving the room ~p~n", [State#state.player_name, State#state.room_id]),
  ulti_room_server:leave(State#state.room_id),
  ok.

%%
%%    Serialization
%%

-spec parse_command(Command::binary()) -> Words::[binary()].
parse_command(Command) ->
  [Word || Word <- binary:split(Command, <<32>>, [global, trim]), byte_size(Word) > 0].

-spec parse_card(binary()) -> card().
parse_card(CardMsg) ->
  [C, N] = binary:split(CardMsg, <<"_">>),
  N2 =
    case N of
      <<"also">> -> 'also';
      <<"felso">> -> 'felso';
      <<"kiraly">> -> 'kiraly';
      <<"asz">> -> 'asz';
      _ ->
        list_to_integer(binary_to_list(N))
    end,
  {binary_to_atom(C, latin1), N2}.

%%-spec convert_card(card()) -> binary().
convert_card({Color, Face}) ->
  list_to_binary([
    atom_to_binary(Color, latin1),
    "_",
    case Face of
      F when is_integer(F) -> integer_to_list(F);
      F when is_atom(F) -> atom_to_list(F)
    end
  ]).

convert_hand(Hand) ->
  list_to_binary([[
    atom_to_list(C),
    "_",
    case N of
      Na when is_atom(N) -> atom_to_list(Na);
      Nn when is_number(N) -> integer_to_list(Nn)
    end,
    " "] || {C, N} <- Hand]).

convert_room_players(Users) ->
  R = lists:foldr(fun({Num, Name}, Acc) -> [integer_to_list(Num), Name | Acc] end, [], Users),
  string:join(R, ", ").

%%
%%   Tests
%%

parse_card_test() -> [
  ?assertEqual({tok, 7}, parse_card(<<"tok_7">>)),
  ?assertEqual({piros, also}, parse_card(<<"piros_also">>))
].

convert_card_test() -> [
  ?assertEqual(<<"tok_felso">>, convert_card({tok, felso})),
  ?assertEqual(<<"makk_8">>, convert_card({makk, 8}))
].

convert_hand_test() ->
  ?assertEqual(<<"tok_asz makk_10 ">>, convert_hand([{tok, asz}, {makk, 10}])).
