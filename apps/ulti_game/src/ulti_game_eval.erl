%% ==================================================================
%% Author: Richard Jonas
%% Description: Evaluates ulti games, rounds.
%% ==================================================================
-module(ulti_game_eval).
-author("Richard_Jonas").

-include("ulti_game.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ==================================================================
%% Type definitions
%% ==================================================================

-type round() :: {Round::1..10, card(), card(), card()}.

%% API
-export([
    evaluate_party/2,
    evaluate_ulti/2,
    evaluate_durchmars/1,
    evaluate_betli/1,
    licit/1
]).

%% ==================================================================
%% API functions
%% ==================================================================

-spec evaluate_party(Gamer::[round()], Opponents::[round()]) ->
    {{winner, gamer | opponents}, 
     {gamer, GamerPoints::integer()}, 
     {opponents, OpponentPoints::integer()}}.
evaluate_party(Gamer, Opponents) ->
    GamerPoints = lists:foldl(
        fun(Take, A) ->
            A + compute_points(Take)
        end,
        0, Gamer),
    OppPoints = lists:foldl(
        fun(Take, A) ->
            A + compute_points(Take)
        end,
        0, Opponents),
  
    Winner =
        if
            GamerPoints > OppPoints -> gamer;
            true -> opponents
        end,
    {{winner, Winner}, {gamer, GamerPoints}, {opponents, OppPoints}}.

-spec evaluate_ulti([round()], face()) -> {winner, gamer | opponents}.
evaluate_ulti(Gamer, Trump) ->
    %% Check if the games won the last round and it contains trump 7.
    case lists:keyfind(10, 1, Gamer) of
        false ->
            {winner, opponents};
        {_, {Trump, 7}, _, _} ->
            {winner, gamer};
        {_, _, {Trump, 7}, _} ->
            {winner, gamer};
        {_, _, _, {Trump, 7}} ->
            {winner, gamer};
        _ ->
        {winner, opponents}
    end.

-spec evaluate_durchmars([round()]) -> {winner, gamer | opponents}.
evaluate_durchmars(Gamer) ->
    if
        length(Gamer) =:= 10 -> {winner, gamer};
        true -> {winner, opponents}
    end.

-spec evaluate_betli([round()]) -> {winner, gamer | opponents}.
evaluate_betli(Gamer) ->
    if
        length(Gamer) =:= 0 -> {winner, gamer};
        true -> {winner, opponents}
    end.

-spec compute_points(Round::round()) -> integer().
compute_points(Take) ->
  {Round, Card1, Card2, Card3} = Take,
  card_points(Card1)
    + card_points(Card2)
    + card_points(Card3)
    + if Round =:= 10 -> 10; true -> 0 end.

-spec card_points(card()) -> 0 | 10.
card_points({_, 10}) -> 10;
card_points({_, asz}) -> 10;
card_points(_) -> 0.

licit([passz]) ->
    10;
licit({piros, [passz]}) ->
    20;
licit([negyven_szaz]) ->
    40;
licit([negy_asz]) ->
    40;
licit([ulti]) ->
    50;
licit([betli]) ->
    50;
licit([durchmars]) ->
    60;
licit([szintelen_durchmars]) ->
    60;
licit([negyven_szaz, negy_asz]) ->
    80;
licit([negyven_szaz, ulti]) ->
    80;
licit({piros, [negyven_szaz]}) ->
    80;
licit([husz_szaz]) ->
    80;
licit([ulti, negy_asz]) ->
    90;
licit({piros, [negy_asz]}) ->
    100;
licit({piros, [ulti]}) ->
    100;
licit([rebetli]) ->
    100;
licit([negyven_szaz, durchmars]) ->
    100;
licit([negyven_szaz, ulti, negy_asz]) ->
    120;
licit([husz_szaz, ulti]) ->
    120;
licit([redurchmars]) ->
    120;
licit({piros, [durchmars]}) ->
    120;
licit([teritett_durchmars]) ->
    120;
licit([negyven_szaz, ulti, durchmars]) ->
    140;
licit([husz_szaz, durchmars]) ->
    140;
licit({piros, [negyven_szaz, ulti]}) ->
    160;
licit({piros, [husz_szaz]}) ->
    160;
licit([negyven_szaz, teritett_durchmars]) ->
    160;
licit([ulti, teritett_durchmars]) ->
    160;
licit({piros, [ulti, negy_asz]}) ->
    180;
licit([husz_szaz, ulti, durchmars]) ->
    180;
licit([negyven_szaz, ulti, teritett_durchmars]) ->
    200;
licit({piros, [negyven_szaz, durchmars]}) ->
    200;
licit({piros, [ulti, durchmars]}) ->
    200;
licit([husz_szaz, teritett_durchmars]) ->
    200;
licit([teritett_betli]) ->
    200;
licit({piros, [negyven_szaz, ulti, negy_asz]}) ->
    240;
licit({piros, [husz_szaz, negy_asz]}) ->
    240;
licit({piros, [husz_szaz, ulti]}) ->
    240;
licit([szintelen_teritett_durchmars]) ->
    240;
licit({piros, [negyven_szaz, ulti, durchmars]}) ->
    280;
licit({piros, [husz_szaz, durchmars]}) ->
    280;
licit({piros, [negyven_szaz, teritett_durchmars]}) ->
    320;
licit({piros, [ulti, teritett_durchmars]}) ->
    320;
licit({piros, [husz_szaz, ulti, durchmars]}) ->
    360;
licit([husz_szaz, ulti, teritett_durchmars]) ->
    360;
licit({piros, [negyven_szaz, ulti, teritett_durchmars]}) ->
    400;
licit({piros, [husz_szaz, teritett_durchmars]}) ->
    400;
licit({piros, [husz_szaz, ulti, teritett_durchmars]}) ->
    480.

%% ==================================================================
%% Tests
%% ==================================================================

-ifdef(TEST).

eval_party_test() ->
  Gamer = [
    {3, {makk,9}, {tok,asz}, {makk,8}},
    {9, {zold,7}, {zold,9}, {zold,10}}
  ],

  Opponents = [
    {5, {tok,9}, {zold,8}, {tok,also}},
    {6, {piros,asz}, {piros,8}, {piros,9}},
    {8, {zold,felso}, {zold,kiraly}, {zold,asz}},
    {10, {zold,also}, {tok,felso}, {tok,7}},
    {1, {makk,felso}, {makk,kiraly}, {makk,10}},
    {2, {makk,asz}, {makk,also}, {makk,7}},
    {4, {piros,7}, {piros,felso}, {piros,10}},
    {7, {piros,kiraly}, {tok,10}, {piros,also}}
  ],

  ?assertEqual({{winner, opponents}, {gamer, 20}, {opponents, 70}}, evaluate_party(Gamer, Opponents)).

-endif.
