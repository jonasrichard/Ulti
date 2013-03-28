%% Copyright
-module(ulti_play).
-author("richard").

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
  init/1, wait_players_card/2,
  beats/2
]).

init([Players, Hands, Extra]) ->
  {ok, wait_players_card, #state{players = Players, hands = Hands,
    extra = Extra, player_no = 1, table = [], takes = {[], [], []}}}.

wait_players_card({put, Card}, State) ->
  {_, _, Hands, _, Table, Takes, No} = State,

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

  %% add to the table
  NewTable = Table ++ [{No, Card}],

  %% if table size == 3 evaluate it, set next player no, add table to the take
  case NewTable of
    _ when length(NewTable) == 3 ->
      {Taker, _} = ulti_misc:which_player_take(NewTable, fun beats/2),
      {next_state, wait_players_card, State#state{
          hands = setelement(No, Hands, NewHand),
          table = [],
          takes = setelement(Taker, Takes, lists:append(element(Taker, Takes), list_to_tuple(NewTable))),
          player_no = Taker
      }};
    _ ->
      {next_state, wait_players_card, State#state{
          hands = setelement(No, Hands, NewHand),
          table = NewTable,
          player_no = No rem 3 + 1
      }}
  end.


%%
%% True if first card cannot be hit by the second one.
%%
beats({Color1, Value1}, {Color2, Value2}) ->
  if Color1 == Color2 ->
    ulti_misc:value_to_number(Value1) > ulti_misc:value_to_number(Value2);
  true ->
    false
  end.
