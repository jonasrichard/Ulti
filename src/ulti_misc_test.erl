%% Copyright
-module(ulti_misc_test).
-author("richard").

-include_lib("eunit/include/eunit.hrl").

-define(
  wtake(C1, F1, C2, F2, C3, F3),
    ulti_misc:which_player_take([{1, {C1, F1}}, {2, {C2, F2}}, {3, {C3, F3}}], nil)
).

-define(
  wtake_low10(C1, F1, C2, F2, C3, F3),
    ulti_misc:which_player_take([{1, {C1, F1}}, {2, {C2, F2}}, {3, {C3, F3}}], low10)
).

-define(
  wtake_trump(C1, F1, C2, F2, C3, F3, Trump),
    ulti_misc:which_player_take([{1, {C1, F1}}, {2, {C2, F2}}, {3, {C3, F3}}], {trump, Trump})
).


beats_test() ->
  ?assert(ulti_misc:beats({tok, felso}, {tok, kiraly})),
  ?assert(ulti_misc:beats({makk, 7}, {makk, also})),

  ?assert(ulti_misc:beats_low10({makk, 7}, {makk, 10})),
  ?assert(ulti_misc:beats_low10({makk, 10}, {makk, also})),

  ?assertNot(ulti_misc:beats({makk, 10}, {makk, felso})),
  ?assertNot(ulti_misc:beats({makk, 10}, {zold, asz})),

  ?assertNot(ulti_misc:beats_low10({tok, kiraly}, {tok, 10})),
  ?assertNot(ulti_misc:beats_low10({piros, kiraly}, {tok, 10})),

  ?assert(ulti_misc:beats_trump({makk, 10}, {zold, 8}, zold)),
  ?assert(ulti_misc:beats_trump({makk, 10}, {makk, asz}, makk)),
  ?assert(ulti_misc:beats_trump({makk, 9}, {makk, also}, piros)),

  ?assertNot(ulti_misc:beats_trump({piros, 8}, {zold, 8}, tok)),
  ?assertNot(ulti_misc:beats_trump({makk, also}, {zold, 8}, makk)).

first_player_take_test() ->
  ?assertEqual({1, {tok, felso}}, ?wtake(tok, felso, zold, 9, zold, asz)),
  ?assertEqual({1, {tok, felso}}, ?wtake(tok, felso, tok, also, piros, asz)),

  ?assertEqual({1, {tok, kiraly}}, ?wtake_low10(tok, kiraly, tok, also, tok, 10)),

  ?assertEqual({1, {tok, kiraly}}, ?wtake_trump(tok, kiraly, tok, also, tok, felso, tok)).

second_player_take_test() ->
  ?assertEqual({2, {makk, also}}, ?wtake(makk, 9, makk, also, makk, 8)),
  ?assertEqual({2, {makk, also}}, ?wtake(makk, 9, makk, also, tok, asz)),

  ?assertEqual({2, {zold, felso}}, ?wtake_low10(zold, 10, zold, felso, zold, also)),
  ?assertEqual({2, {zold, felso}}, ?wtake_low10(zold, also, zold, felso, zold, 10)),

  ?assertEqual({2, {makk, also}}, ?wtake_trump(tok, kiraly, makk, also, tok, felso, makk)),
  ?assertEqual({2, {makk, also}}, ?wtake_trump(tok, kiraly, makk, also, makk, 9, makk)),
  ?assertEqual({2, {makk, also}}, ?wtake_trump(makk, 8, makk, also, tok, felso, makk)),
  ?assertEqual({2, {makk, also}}, ?wtake_trump(makk, 8, makk, also, makk, 9, makk)).

third_player_take_test() ->
  ?assertEqual({3, {piros, kiraly}}, ?wtake(piros, 8, zold, 9, piros, kiraly)),
  ?assertEqual({3, {piros, kiraly}}, ?wtake(piros, 8, piros, 9, piros, kiraly)),
  ?assertEqual({3, {piros, kiraly}}, ?wtake(piros, also, piros, 7, piros, kiraly)),

  ?assertEqual({3, {makk, also}}, ?wtake_low10(makk, 10, makk, 8, makk, also)),
  ?assertEqual({3, {makk, also}}, ?wtake_low10(makk, 7, makk, 10, makk, also)),

  ?assertEqual({3, {piros, felso}}, ?wtake_trump(makk, 8, makk, also, piros, felso, piros)),
  ?assertEqual({3, {piros, felso}}, ?wtake_trump(piros, 8, makk, also, piros, felso, piros)),
  ?assertEqual({3, {piros, felso}}, ?wtake_trump(makk, 8, piros, also, piros, felso, piros)).
