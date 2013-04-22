%% Copyright
-module(ulti_player).
-author("Richard_Jonas").
-behaviour(gen_fsm).

-include("ulti_game.hrl").

-export([init/1, wait_for_events/2, wait_for_card/2]).

-record(state, {
  handler       :: pid(),             %% Pid of the websocket handler
  game_pid      :: pid(),             %% Pid of the ulti game fsm
  hand          :: hand(),
  gamer         :: boolean(),         %% Is he the gamer (announced the game)
  pair          :: pid() | nil,       %% Pair if they are catcher otherwise nil
  takes         :: [take()],          %% All the rounds he took
  modifiers     :: [game_mod()]       %% 20, 40, kontra, etc.
}).

init(Args) ->
  {ok, wait_for_players, #state{}}.

%% Wait for players to put card. When they finish we can put. Let us notify our player that
%% he can put a card.

wait_for_events(you_can_put_card, State) ->
  State#state.handler ! {you_can_put_card},
  {next_state, wait_for_card, State};

wait_for_events({put, Player, Card}, State) ->
  {next_state, wait_for_events, State};

wait_for_events({you_took, Take}, State) ->
  {next_state, wait_for_events, State};

wait_for_events(_, State) ->
  {stop, unknown_message, State}.

%% Our player just put a card onto the table. Let us notify the game about that
wait_for_card({put, Card}, State) ->
  State#state.game_pid ! {put, self(), Card},



