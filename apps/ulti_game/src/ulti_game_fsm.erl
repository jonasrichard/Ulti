-module(ulti_game_fsm).
-behaviour(gen_fsm).

-export([
        start/0,
        start_link/0,
        get_state/1
    ]).

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
        licit               :: {Player :: 1..3, licit()}
    }).

%%%============================================================================
%%% External API
%%%============================================================================

start() ->
    gen_fsm:start(?MODULE, [], []).

start_link() ->
    gen_fsm:start(?MODULE, [], []).

get_state(FsmRef) ->
    gen_fsm:sync_send_all_state_event(FsmRef, get_state).

%%%============================================================================
%%% gen_fsm callbacks
%%%============================================================================

init(Args) ->
    {ok, wait_for_connect, #state{}}.

handle_event(Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(get_state, _From, StateName, State) ->
    {reply, {StateName, State}, StateName, State};
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
            %% Check licit
            
            NewLicit =
                case State#state.licit of
                    undefined ->
                        {Current, Licit};
                    {Owner, CurrentLicit} ->
                        Val1 = ulti_game_eval:licit_value(Licit),
                        Val2 = ulti_game_eval:licit_value(CurrentLicit),
                        if  Val1 > Val2 ->
                                {Current, Licit};
                            true ->
                                {Owner, CurrentLicit}
                        end;
                    CurrentLicit ->
                        CurrentLicit
                end,

            %% TODO: nofity_players({PlayerName, Licit, Value})

            {next_state, wait_for_licit,
                State#state{
                    current_player = next_player(Current),
                    licit = NewLicit
                }};
        _ ->
            %% error, it is not your turn
            %% notify_player(WebSocket, not_your_turn)
            {next_state, wait_for_licit, State}
    end;

wait_for_licit(_Event, State) ->
    {next_state, wait_for_licit, State}.

%%%============================================================================
%%% Internal functions 
%%%============================================================================

next_player(N) ->
    (N rem 3) + 1.
