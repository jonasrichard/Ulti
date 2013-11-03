-module(ulti_game_tests).

-include_lib("proper/include/proper.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type color() :: ulti_game_play:color().
-type face()  :: ulti_game_play:face().
-type card()  :: ulti_game_play:card().

-ifdef(TEST).

proper_module_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}])).

prop_beat_same_color() ->
    ?FORALL({Color, Face1, Face2}, {color(), face(), face()},
        ?IMPLIES(Face1 =/= Face2,
            begin
                Card1 = {Color, Face1},
                Card2 = {Color, Face2},
                case ulti_game_play:beat(Card1, Card2) of
                    true ->
                        not ulti_game_play:beat(Card2, Card1);
                    false ->
                        ulti_game_play:beat(Card2, Card1)
                end
            end
        )
    ).

prop_beat_diff_color() ->
    ?FORALL({{Color1, Face1}, {Color2, Face2}},
            {{color(), face()}, {color(), face()}},
        ?IMPLIES(Color1 =/= Color2 andalso Face1 =/= Face2,
            begin
                Card1 = {Color1, Face1},
                Card2 = {Color2, Face2},
                not ulti_game_play:beat(Card1, Card2)
            end
        )
    ).

-endif.

