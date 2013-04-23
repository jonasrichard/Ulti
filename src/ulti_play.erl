%% Copyright
-module(ulti_play).
-author("richard").
-include("ulti_game.hrl").

-behaviour(gen_fsm).

-record(state, {
  players          :: [player()],
  table            :: [{pid(), card()}],
  current_player   :: pid(),
  round = 1        :: 1..10,
  gamer_takes = [] :: [take()],
  opponents = []   :: [take()]
}).

%% API
-export([
  start_game/1,
  init/1, wait_players_card/2, collect_takes/2, terminate/3
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
  Players = State#state.players,
  Current = State#state.current_player,

  Pid = Current,

  [gen_event:notify(PPid, {put, Name, Card}) || {Name, PPid} <- Players, PPid =/= Current],

  %% add to the table
  NewTable = State#state.table ++ [{Current, Card}],

  %% if table size == 3 evaluate it, set next player no, add table to the take
  case NewTable of
    _ when length(NewTable) == 3 ->
      {Taker, _} = ulti_misc:which_player_take(NewTable, {trump, tok}),
      error_logger:info_msg("Table: ~w take ~w~n", [NewTable, Taker]),

      Round = State#state.round,

      %% Notify players
      TakerName = lists:keyfind(Taker, 2, Players),
      [if
        Player =:= Taker -> gen_event:notify(Player, {take, {Round, NewTable}});
        true -> gen_event:notify(Player, {taker, TakerName})
      end
        || {_Name, Player} <- Players],

      if
        Round =:= 10 ->
          [gen_event:notify(Player, game_end )|| {_, Player} <- Players],
          {next_state, collect_takes, State#state{table = [], current_player = Taker}};
        true ->
          {next_state, wait_players_card, State#state{
            table = [],
            current_player = Taker,
            round = Round + 1}}
      end;
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

collect_takes(Msg, State) ->
  NewState =
    case Msg of
      {gamer, Pid, Takes} ->
        State#state{current_player = Pid, gamer_takes = Takes};
      {opponent, Takes} ->
        State#state{opponents = State#state.opponents ++ Takes}
    end,

  if
    length(State#state.gamer_takes) + length(State#state.opponents) =:= 10 ->
      {stop, normal, NewState};
    true ->
      {next_state, collect_takes, NewState}
  end.

terminate(normal, _, State) ->
  error_logger:info_msg("Evaluate ~w~n", [State]),
  ok;
terminate(_, _, _) ->
  ok.

-spec deal([pid()]) -> any().
deal(Players) ->
  {H1, H2, H3} = ulti_misc:deal(),
  HH1 = tl(tl(H1)),
  lists:zipwith(fun(P, H) -> gen_event:notify(P, {deal, H}) end, Players, [HH1, H2, H3]).

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

