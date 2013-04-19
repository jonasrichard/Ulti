%% Copyright
-module(ulti_play).
-author("richard").
-include("ulti_game.hrl").

-behaviour(gen_fsm).

-record(state, {
  players,
  hands,           %% tuple of list of cards
  extra,
  table,           %% [{2, {tok, 10}}, {3, {zold, 9}] list of number of player and its card
  takes,           %% tuple of taken cards [{player_no, card}, ...]
  player_no
}).

%% API
-export([
  start_game/1,
  init/1, wait_players_card/2, terminate/3,
  beats/2
]).

-spec start_game([{string(), pid()}]) -> any().
start_game(Users) ->
  Players = list_to_tuple([Pid || {_PlayerName, Pid} <- Users]),
  {H1, H2, H3} = ulti_misc:deal(),
  [E1, E2 | H11] = H1,
  error_logger:info_msg("Starting game ~n~p~n~p~n~p~n", [H11, H2, H3]),
  gen_fsm:start(ulti_play, [Players, {H11, H2, H3}, [E1, E2]], []).

init([Players, Hands, Extra]) ->
  PlayerList = tuple_to_list(Players),
  [P ! {init, self(), H} || {P, H} <- lists:zip(PlayerList, tuple_to_list(Hands))],

  [P ! {start, 1} || P <- PlayerList],

  {ok, wait_players_card, #state{players = Players, hands = Hands,
    extra = Extra, player_no = 1, table = [], takes = {[], [], []}}}.

wait_players_card({put, Card}, State) ->
  {state, Players, Hands, _, Table, Takes, No} = State,

  Hand = element(No, Hands),

  %% check if Pack contains card
  case lists:member(Card, Hand) of
    false ->
      error_logger:error_msg("Invalid card ~p, not in player's hand~n", [Card]),
      {ok, wait_players_card, State};
    _ ->
      ok
  end,

  %% delete Card
  NewHand = lists:delete(Card, Hand),

  element(No, Players),

  lists:zipwith(
    fun(Num, Pid) ->
      case Num of
        No ->
          Pid ! {hand, NewHand};
        _ ->
          Pid ! {other_hand, No, length(NewHand)},
          Pid ! {put, No, Card}
      end
    end,
    lists:seq(1, size(Players)),
    tuple_to_list(Players)),

  %% add to the table
  NewTable = Table ++ [{No, Card}],

  %% if table size == 3 evaluate it, set next player no, add table to the take
  case NewTable of
    _ when length(NewTable) == 3 ->
      {Taker, _} = ulti_misc:which_player_take(NewTable, fun beats/2),
      error_logger:info_msg("Table: ~w take ~w~n", [NewTable, Taker]),

      %% Notify players
      [Player ! {take, Taker} || Player <- tuple_to_list(Players)],

      {next_state, wait_players_card, State#state{
          hands = setelement(No, Hands, NewHand),
          table = [],
          takes = setelement(Taker, Takes, lists:append(element(Taker, Takes), [list_to_tuple(NewTable)])),
          player_no = Taker
      }};
    _ ->
      {next_state, wait_players_card, State#state{
          hands = setelement(No, Hands, NewHand),
          table = NewTable,
          player_no = No rem 3 + 1
      }}
  end.

terminate(normal, _, State) ->
  %% evaluate game
  ok;
terminate(_, _, _) ->
  ok.

%%
%% True if first card cannot be hit by the second one.
%%
-spec beats(card(), card()) -> boolean().
beats({Color1, Value1}, {Color2, Value2}) ->
  if Color1 == Color2 ->
    ulti_misc:value_to_number(Value1) > ulti_misc:value_to_number(Value2);
  true ->
    false
  end.

%%-spec put_card_from_hand(Card, Hands, PlayerNumber) -> NewHands when
put_card_from_hand(Card, Hands, PlayerNumber) ->
  Hand = element(PlayerNumber, Hands),
  NewHand = lists:delete(Card, Hand),
  setelement(PlayerNumber, Hands, NewHand).

