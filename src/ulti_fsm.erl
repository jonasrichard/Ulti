%% Copyright
-module(ulti_fsm).
-author("Richard_Jonas").

-behaviour(gen_fsm).

-record(players, {player1, player2, player3}).

-record(game, {
  type,           %% list of passz, negyven_szaz, negy_asz, ulti, betli, durtmars, szintelen_durtmars,
                  %% husz_szaz, teritett_betli, teritett_durtmars
  color,          %% undefined, piros or sometimes tok, zold, makk
  options         %% like [{kontra, game}, {rekortra, party}]  szubkontra, mordkontra, fedák sári
}).

-record(state, {
  players,                       %% pid of the three players
  pack1, pack2, pack3, extra,    %% cards of the three players and the talon (extra cards)
  caller,                        %% The no. of the player who actually calls
  game                           %% which game the caller will play
}).

%% API
-export([
  init/1,
  wait_for_players_to_connect/2, wait_for_licit/2
]).

init(_) ->
  {ok, wait_for_players_connect, #state{}}.

wait_for_players_to_connect({connect, PlayerPid}, State) ->
  Players = State#state.players,
  case Players of
    {undefined, _, _} ->
      {next_state, wait_for_players_to_connect, State#state{players = Players#players{player1 = PlayerPid}}};
    {_, undefined, _} ->
      {next_state, wait_for_players_to_connect, State#state{players = Players#players{player2 = PlayerPid}}};
    {_, _, undefined} ->
      {Pack1, Pack2, Pack3} = ulti_misc:deal(),
      NewState = State#state{
          players = Players#players{player3 = PlayerPid},
          pack1 = Pack1, pack2 = Pack2, pack3 = Pack3,
          caller = 1
          },
      {next_state, wait_for_licit, NewState}
  end.

wait_for_licit({licit, Player, Game}, State) ->
  PlayerNo = player_to_number(Player, State#state.players),

  ok.

%% Private functions

player_to_number(Pid, Players) ->
  case Players of
    {Pid, _, _} ->
      1;
    {_, Pid, _} ->
      2;
    {_, _, Pid} ->
      3
  end.
