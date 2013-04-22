%% Copyright
-module(ulti_play).
-author("richard").
-include("ulti_game.hrl").

-behaviour(gen_fsm).

-record(state, {
  players          :: [player()],
  table            :: [{pid(), card()}],
  current_player   :: pid()
}).

%% API
-export([
  start_game/1,
  init/1, wait_players_card/2, terminate/3,
  beats/2
]).

-spec start_game([{string(), pid()}]) -> pid().
start_game(Players) ->
  {ok, Fsm} = gen_fsm:start(?MODULE, Players, []),
  Fsm.

init(Players) ->
  {Names, Handlers} = lists:unzip(Players),

  PlayerPids = lists:map(
    fun(HandlerPid) ->
      ulti_player:start(HandlerPid, self(), HandlerPid =:= hd(Handlers))
    end,
    Handlers),

  Gamer = hd(PlayerPids),

  [P1, P2] = tl(PlayerPids),
  gen_event:notify(P1, {pair, P2}),
  gen_event:notify(P2, {pair, P1}),

  deal(PlayerPids),

  gen_event:notify(Gamer, you_can_put_card),

  {ok, wait_players_card, #state{
    players = lists:zip(Names, PlayerPids),
    table = [],
    current_player = hd(PlayerPids)
  }}.

wait_players_card({put, Pid, Card}, State) ->
  {state, Players, Table, Current} = State,

  Pid = Current,

  [gen_event:notify(Pid, {put, Name, Card}) || {Name, Pid} <- Players, Pid =/= Current],

  %% add to the table
  NewTable = Table ++ [{Current, Card}],

  %% if table size == 3 evaluate it, set next player no, add table to the take
  case NewTable of
    _ when length(NewTable) == 3 ->
      {Taker, _} = ulti_misc:which_player_take(NewTable, fun beats/2),
      error_logger:info_msg("Table: ~w take ~w~n", [NewTable, Taker]),

      %% Notify players
      TakerName = lists:keyfind(Taker, 2, Players),
      [if
        Player =:= Taker -> gen_event:notify(Player, {take, NewTable});
        true -> gen_event:notify(Player, {taker, TakerName})
      end
        || {_Name, Player} <- Players],

      {next_state, wait_players_card, State#state{table = [], current_player = Taker}};
    _ ->
      NextPlayer =
        case Players of
          [{_, Current}, {_, P}, _] -> P;
          [_, {_, Current}, {_, P}] -> P;
          [{_, P}, _, {_, Current}] -> P
        end,
      gen_event:notify(NextPlayer, you_can_put_card),
      {next_state, wait_players_card, State#state{table = NewTable, current_player = NextPlayer}}
  end.

terminate(normal, _, State) ->
  %% evaluate game
  ok;
terminate(_, _, _) ->
  ok.

-spec deal([pid()]) -> any().
deal(Players) ->
  {H1, H2, H3} = ulti_misc:deal(),
  HH1 = tl(tl(H1)),
  lists:zipwith(fun(P, H) -> gen_event:notify(P, {deal, H}) end, Players, [HH1, H2, H3]).

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

%% TODO: who is the gamer? Players' points against him will be added!
%% {win, party, 70}, {win, silent_ulti}
%% silent ulti, silent 100, silent durchmars
-spec evaluate_game({[take()], [take()], [take()]}) -> any().
evaluate_game(AllTakes) ->
  PartyPoints =
    lists:map(
      fun(Takes) ->
        lists:foldl(
          fun({Round, Cards}, Acc1) ->
            lists:foldl(
              fun({_, 10}, Acc2)  -> Acc2 + 10;
                 ({_, asz}, Acc2) -> Acc2 + 10;
                 (_, Acc2)        -> Acc2
              end,
              if Round =:= 10 -> Acc1 + 10; true -> Acc1 end,
              Cards
            )
          end,
          0,
          Takes
        )
      end,
      tuple_to_list(AllTakes)
    ),
  MaxPoint = lists:max(PartyPoints)
  .

