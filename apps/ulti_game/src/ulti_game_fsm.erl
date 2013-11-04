-module(ulti_game_fsm).
-behaviour(gen_fsm).

-export([
        init/1,
        handle_event/3,
        handle_info/3,
        handle_sync_event/4,
        code_change/4,
        terminate/3
     ]).

-export([
        wait_for_connect/2,
        wait_for_licit/2
    ]).

-include_lib("ulti_game/include/ulti_game.hrl").

-record(state, {
        players = []        :: [{Name :: string(), WebSocket :: pid()}],
        current_player = 1  :: 1..3,
        licit               :: licit()
    }).

%%=============================================================================
%% gen_fsm callbacks
%%=============================================================================

init(Args) ->
    {ok, wait_for_connect, #state{}}.

handle_event(Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(Event, From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(Info, StateName, State) ->
    {next_state, StateName, State}.

code_change(OldVsn, StateName, State, Extra) ->
    {ok, StateName, State}.

terminate(Reason, StateName, State) ->
    ok.

%%=============================================================================
%% fsm state transitions 
%%=============================================================================

%% Wait for users to connect to the game

wait_for_connect({connect, UserName, WebSocket}, State) ->
    NewPlayers = State#state.players ++ [{UserName, WebSocket}],
    NewState = State#state{players = NewPlayers},

    WebSocket ! {connected, self()},

    case length(NewPlayers) of
        3 ->
            {next_state, wait_for_licit, NewState};
        _ ->
            {next_state, wait_for_connect, NewState}
    end;

wait_for_connect(_Event, State) ->
    {next_state, wait_for_connect, State}.

%% current_player can licit

wait_for_licit({licit, WebSocket, Licit}, State) ->
    #state{current_player = Current, players = Players} = State,
    case lists:nth(Current, Players) of
        {UserName, WebSocket} ->
            %% check_licit,
            %% send message to websocket
            ok;
        _ ->
            %% error, it is not your turn
            ignore
    end;

wait_for_licit(_Event, State) ->
    {next_state, wait_for_licit, State}.

