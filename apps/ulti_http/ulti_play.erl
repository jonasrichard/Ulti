%% Copyright
-module(ulti_game_play).
-author("richard").

-behaviour(gen_fsm).

-include("ulti_game.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
    players          :: [player()],
    game             :: [game()],
    table            :: [{pid(), card()}],
    current_player   :: pid(),
    round = 1        :: 1..10,
    gamer_takes = [] :: [take()],
    opponents = []   :: [take()]
}).

%% API
-export([start_game/1]).

%% Callback functions
-export([
        init/1,
        wait_players_card/2, collect_takes/2,
        terminate/3
    ]).

-spec start_game([{string(), pid()}]) -> pid().
start_game(Players) ->
  {ok, Fsm} = gen_fsm:start(?MODULE, Players, []),
  Fsm.

%% ==================================================================
%% Callback functions
%% ==================================================================

init(Players) ->
    {Names, Handlers} = lists:unzip(Players),

    %% Start player event listeners for each player
    PlayerPids = lists:map(
        fun(HandlerPid) ->
            ulti_player:start(HandlerPid, self(), HandlerPid =:= hd(Handlers))
        end,
        Handlers),

    %% Let the first player start the play (party right now)
    Gamer = hd(PlayerPids),

    %% The other two players are on the same side (catchers)
    [P1, P2] = tl(PlayerPids),
    gen_event:notify(P1, {pair, P2}),
    gen_event:notify(P2, {pair, P1}),

    %% Deal the cards
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
          [gen_event:notify(Player, game_end)|| {_, Player} <- Players],
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
  end;

wait_players_card({bela, Pid, Bela}, State) ->
  Players = State#state.players,
  {Name, _} = lists:keyfind(Pid, 2, Players),
  [gen_event:notify(PlayerPid, {bela, Name, Bela}) || {_N, PlayerPid} <- Players],
  {next_state, wait_players_card, State};

wait_players_card({kontra, Pid, Game}, State) ->
  NewGame =
    case lists:keyfind(Game, 1, State#state.game) of
      false ->
        no_such_game;
      {Game, _Color, KontraList} ->
        NewKontra =
          case lists:keyfind(Pid, 2, KontraList) of
            false ->
              {kontra, Pid};
            {K, Pid} ->
              ulti_misc:next_kontra(K)
          end,
        lists:keyreplace(Game, 1, State#state.game, NewKontra)
    end,
  {next_state, wait_players_card, State#state{game = NewGame}}.

collect_takes(Msg, State) ->
    NewState =
        case Msg of
            {gamer, Pid, Takes} ->
                State#state{current_player = Pid, gamer_takes = Takes};
            {opponent, Takes} ->
                State#state{opponents = State#state.opponents ++ Takes}
        end,

    if
        length(NewState#state.gamer_takes) + length(NewState#state.opponents) =:= 10 ->
            {stop, normal, NewState};
        true ->
            {next_state, collect_takes, NewState}
  end.

terminate(normal, _, State) ->
  error_logger:info_msg("Evaluate ~w~n", [State]),
  evaluate_game(State#state.gamer_takes, State#state.opponents),
  ok;
terminate(_, _, _) ->
  ok.

-spec deal([pid()]) -> any().
deal(Players) ->
    {H1, H2, H3} = ulti_misc:deal(),
    HH1 = tl(tl(H1)),
    lists:zipwith(
        fun(P, H) ->
            gen_event:notify(P, {deal, H})
        end,
        Players, [HH1, H2, H3]).

evaluate_game(Gamer, Opponents) ->
  {Winner, GamerPoints, OpponentPoints} = ulti_eval:evaluate_party(Gamer, Opponents),
  ulti_eval:evaluate_ulti(Gamer, tok),
  ulti_eval:evaluate_durchmars(Gamer).

