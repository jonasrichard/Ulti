-module(ulti_game_fsm_SUITE).

-export([
        all/0,
        connect/1
    ]).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [connect].

connect(_) ->
    {ok, Game} = gen_fsm:start_link(ulti_game_fsm, [], []),
    TestProcess = self(),
    ct:log("Test process is ~p", [TestProcess]),

    Joe = spawn_link(fun() -> player(TestProcess) end),
    Jack = spawn_link(fun() -> player(TestProcess) end),
    Jill = spawn_link(fun() -> player(TestProcess) end),

    ct:log("Joe is connecting..."),
    gen_fsm:send_event(Game, {connect, "Joe", self()}),
    wait_for_connect = get_fsm_statename(Game),
    got_message(Joe, {connected, Game}),

    ct:log("Jack is connecting..."),
    gen_fsm:send_event(Game, {connect, "Jack", self()}),
    wait_for_connect = get_fsm_statename(Game),
    got_message(Jack, {connected, Game}),
    
    ct:log("Jill is connecting..."),
    gen_fsm:send_event(Game, {connect, "Jill", self()}),
    wait_for_licit = get_fsm_statename(Game),
    got_message(Jill, {connected, Game}),

    ok.

get_fsm_statename(Pid) ->
    proplists:get_value("StateName", get_fsm_state(Pid)).

%% Results in a proplist like
%%     [{"StateName", wait_for_connect}, "Status", running}]
get_fsm_state(Pid) ->
    {status, _, _, Items} = sys:get_status(Pid),
    lists:foldl(
        fun(E, Acc) when is_list(E) ->
            case proplists:get_value(data, E) of
                undefined ->
                    Acc;
                Value when is_list(Value) ->
                    Value ++ Acc;
                Value ->
                    [Value | Acc]
            end;
           (E, Acc) ->
               Acc
        end, [], Items).

got_message(Pid, Message) ->
    receive
        Message ->
            ct:pal("~p got ~p", [Pid, Message]),
            true;
        E ->
            ct:fail("~p has got ~p (expected ~p)",
                [Pid, E, Message])
    after 5000 ->
            ct:fail("~p timeout during waiting for ~p", [Pid, Message])
    end.

player(TestProcess) ->
    receive
        Message ->
            ct:log("Player ~p got ~p", [self(), Message]),
            TestProcess ! {self(), Message},
            player(TestProcess)
    end.
