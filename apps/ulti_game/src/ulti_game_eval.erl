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
    evaluate_betli/1
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

%%-spec convert_result([take()], [take()]) -> {[round()], [round()]}.
%%convert_result(Gamer, Opponent) ->
%%  {[take_to_round(T) || T <- Gamer], [take_to_round(T) || T <- Opponent]}.
%%
%%-spec take_to_round(take()) -> round().
%%take_to_round(Take) ->
%%  {Round, [{_, Card1}, {_, Card2}, {_, Card3}]} = Take,
%%  {Round, Card1, Card2, Card3}.

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
