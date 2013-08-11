%% Copyright
-module(ulti_player).
-author("Richard_Jonas").
-behaviour(gen_event).

-include("ulti_game.hrl").

-export([start/3]).
-export([init/1, handle_event/2]).

-record(state, {
    handler       :: pid(),           %% Pid of the websocket handler
    game_pid      :: pid(),           %% Pid of the ulti game fsm
    hand          :: hand(),
    gamer         :: boolean(),       %% Is he the gamer (announced the game)
    pair          :: pid() | nil,     %% Pair if they are catcher otherwise nil
    takes         :: [take()],        %% All the rounds he took
    bela          :: [bela()]         %% List of bela (20, 40)
}).

-spec start(Handler::pid(), GamePid::pid(), Gamer::boolean()) -> pid().
start(Handler, GamePid, Gamer) ->
    {ok, Pid} = gen_event:start(),
    gen_event:add_handler(Pid, ?MODULE, {Handler, GamePid, Gamer}),
    Pid.

init({Handler, GamePid, Gamer}) ->
    Handler ! {set_event_handler, self()},
    {ok, #state{
            handler = Handler,
            game_pid = GamePid,
            hand = [],
            gamer = Gamer,
            pair = nil,
            takes = []}}.

handle_event({pair, Pair}, State) ->
    {ok, State#state{pair = Pair}};

handle_event({deal, Cards}, State) ->
    State#state.handler ! {hand, Cards},
    {ok, State#state{hand = Cards}};

%% We can put a card, let us notify ws_handler about that.
handle_event(you_can_put_card, State) ->
    State#state.handler ! you_can_put_card,
    {ok, State};

%% Our player put a card
handle_event({put, Card}, State) ->
    NewHand = lists:delete(Card, State#state.hand),
    gen_fsm:send_event(State#state.game_pid, {put, self(), Card}),
    State#state.handler ! {hand, NewHand},
    {ok, State#state{hand = NewHand}};

handle_event({put, PlayerName, Card}, State) ->
    State#state.handler ! {put, PlayerName, Card},
    {ok, State};

handle_event({bela, BelaList}, State) ->
    lists:foreach(
        fun(Bela) ->
            gen_fsm:send_event(State#state.game_pid, {bela, self(), Bela})
        end,
        BelaList
    ),
    {ok, State#state{bela = BelaList}};

handle_event({kontra, Game}, State) ->
    gen_fsm:send_event(State#state.game_pid, {kontra, self(), Game}),
    {ok, State};

handle_event({take, Take}, State) ->
    NewTakes = State#state.takes ++ [Take],
    State#state.handler ! take,
    {ok, State#state{takes = NewTakes}};

handle_event({taker, PlayerName}, State) ->
    State#state.handler ! {taker, PlayerName},
    {ok, State};

handle_event(game_end, #state{game_pid = GamerPid} = State) ->
    if
        State#state.gamer ->
            gen_fsm:send_event(GamerPid, {gamer, self(), State#state.takes});
        true ->
            gen_fsm:send_event(GamerPid, {opponent, State#state.takes})
    end,
    {ok, State}.

