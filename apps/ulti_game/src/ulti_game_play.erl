%%%==================================================================
%%% @copyright 2012-2013 Richard Jonas
%%% @author: Richard Jonas <richard.jonas.76@gmail.com>
%%% @doc 
%%%
%%%==================================================================
-module(ulti_game_play).

%% API exports
-export([
        beat/2, beat_low10/2
    ]).

-include_lib("ulti_game/include/ulti_game.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%==================================================================
%%% API functions
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc
%% It is true if the first card beats the second card.
%% @end
%%-------------------------------------------------------------------
-spec beat(Card :: card(), OtherCard :: card()) -> boolean().
beat(Card, {Color, Face} = _OtherCard) ->
    case Card of
        {Color, Face2} ->
            to_number(Face2) > to_number(Face);
        _ ->
            false
    end.

%%-------------------------------------------------------------------
%% @doc
%% It is true if the first card beats the second one and the
%% 10 is low (betli, szintelen durchmars).
%% @end
%%-------------------------------------------------------------------
-spec beat_low10(Card :: card(), OtherCard :: card()) -> boolean().
beat_low10(Card, {Color, Face} = _OtherCard) ->
    case Card of
        {Color, Face2} ->
            to_number_low10(Face2) > to_number_low10(Face);
        _ ->
            false
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================

%% @doc Convert face to numberic value
-spec to_number(face()) -> 1..8.
to_number(7) ->
  1;
to_number(8) ->
  2;
to_number(9) ->
  3;
to_number(also) ->
  4;
to_number(felso) ->
  5;
to_number(kiraly) ->
  6;
to_number(10) ->
  7;
to_number(asz) ->
  8.

-spec to_number_low10(face()) -> 1..8.
to_number_low10(7) ->
  1;
to_number_low10(8) ->
  2;
to_number_low10(9) ->
  3;
to_number_low10(10) ->
  4;
to_number_low10(also) ->
  5;
to_number_low10(felso) ->
  6;
to_number_low10(kiraly) ->
  7;
to_number_low10(asz) ->
  8.

%%%==================================================================
%%% Tests
%%%==================================================================

-ifdef(TEST).

beat_test() ->
    [?_assert(beat({tok, 8}, {tok, 7})),
     ?_assert(beat({tok, asz}, {tok, kiraly})),
     ?_assert(beat({makk, 10}, {makk, kiraly})),
     ?_assertNot(beat({tok, 7}, {tok, 9})),
     ?_assertNot(beat({tok, asz}, {piros, 7}))].

beat_low10_test() ->
    [?_assert(beat({tok, 10}, {tok, 9})),
     ?_assertNot(beat({tok, 10}, {tok, also}))].

-endif.

