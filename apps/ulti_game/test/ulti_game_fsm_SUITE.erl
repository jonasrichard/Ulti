-module(ulti_game_fsm_SUITE).

-export([
        all/0,
        connect/1
    ]).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [connect, licit].

init_per_suite(Config) ->
    {ok, Game} = ulti_game_fsm:start(),
    ct:pal("Starting players..."),

    Joe = spawn(fun() -> player(undefined) end),
    Jack = spawn(fun() -> player(undefined) end),
    Jill = spawn(fun() -> player(undefined) end),

    [{game, Game}, {players, [Joe, Jack, Jill]} | Config].

connect(Config) ->
    Game = ?config(game, Config),
    [Joe, Jack, Jill] = ?config(players, Config),

    got_message(Joe, {connected, Game},
            fun() -> gen_fsm:send_event(Game, {connect, "Joe", Joe}) end),
    {wait_for_connect, _} = ulti_game_fsm:get_state(Game),

    got_message(Jack, {connected, Game},
            fun() -> gen_fsm:send_event(Game, {connect, "Jack", Jack}) end),
    {wait_for_connect, _} = ulti_game_fsm:get_state(Game),
    
    got_message(Jill, {connected, Game},
            fun() -> gen_fsm:send_event(Game, {connect, "Jill", Jill}) end),
    {wait_for_licit, _} = ulti_game_fsm:get_state(Game).

licit(Config) ->
    Game = ?config(game, Config),
    [Joe, Jack, Jill] = ?config(players, Config),

    gen_fsm:send_event(Game, {licit, Joe, [passz]}),
    {wait_for_licit, State1} = ulti_game_fsm:get_state(Game),
    
    2 = element(3, State1),
    {1, [passz]} = element(4, State1),

    gen_fsm:send_event(Game, {licit, Jack, [passz]}),
    {wait_for_licit, State2} = ulti_game_fsm:get_state(Game),
    
    3 = element(3, State2),
    {1, [passz]} = element(4, State2).
    
%%%============================================================================
%%% Internal helper functions
%%%============================================================================

got_message(Pid, Message, Fun) ->
    Pid ! {callback, {self(), Message}},
    Fun(),
    receive
        Message ->
            ct:pal("~p got ~p", [Pid, Message]),
            true
    after 5000 ->
            ct:fail("~p timeout during waiting for ~p", [Pid, Message])
    end.

player(Callback) ->
    receive
        {callback, NewCallback} ->
            player(NewCallback);
        Message ->
            ct:pal("Player ~p got ~p", [self(), Message]),
            case Callback of
                undefined ->
                    player(undefined);
                {Pid, Message} ->
                    Pid ! Message,
                    player(undefined);
                _ ->
                    player(Callback)
            end
    end.
