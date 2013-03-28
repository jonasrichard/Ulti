%% Copyright
-module(ulti_misc).
-author("Richard_Jonas").

%% API
-export([deal/0, sort/1, compute_game_money/1]).

deal() ->
  deal([], [], [], generate_cards()).

deal(First, Second, Third, Pack) when length(Pack) > 2 ->
  Num = length(Pack),
  Card = lists:nth(random:uniform(Num), Pack),
  case Num rem 3 of
    0 ->
      deal([Card | First], Second, Third, lists:delete(Card, Pack));
    1 ->
      deal(First, [Card | Second], Third, lists:delete(Card, Pack));
    2 ->
      deal(First, Second, [Card | Third], lists:delete(Card, Pack))
  end;
deal(First, Second, Third, Pack) ->
  {sort(First ++ Pack), sort(Second), sort(Third)}.

generate_cards() ->
  [{X,Y} || X <- [tok, makk, piros, zold], Y <- [7, 8, 9, also, felso, kiraly, 10, asz]].

color_to_number(tok) ->
  1;
color_to_number(makk) ->
  2;
color_to_number(zold) ->
  3;
color_to_number(piros) ->
  4.

value_to_number(7) ->
  1;
value_to_number(8) ->
  2;
value_to_number(9) ->
  3;
value_to_number(also) ->
  4;
value_to_number(felso) ->
  5;
value_to_number(kiraly) ->
  6;
value_to_number(10) ->
  7;
value_to_number(asz) ->
  8.

compare_cards({Color1, Value1}, {Color2, Value2}) ->
  N1 = color_to_number(Color1),
  N2 = color_to_number(Color2),
  if N1 < N2 ->
    true;
  true ->
      if N1 > N2 ->
        false;
      true ->
        value_to_number(Value1) =< value_to_number(Value2)
      end
  end.

sort(Cards) ->
  lists:sort(fun compare_cards/2, Cards).

compute_game_money([]) ->
  0;
compute_game_money([H|T]) ->
  compute_game_money(H) + compute_game_money(T);
compute_game_money({passz}) ->
  10;
compute_game_money({passz, piros}) ->
  20;
compute_game_money({ulti}) ->
  40;
compute_game_money({ulti, piros}) ->
  80;
compute_game_money({negyven_szaz}) ->
  40;
compute_game_money({negyven_szaz, piros}) ->
  80;
compute_game_money({husz_szaz}) ->
  80;
compute_game_money({husz_szaz, piros}) ->
  160;
compute_game_money({betli}) ->
  50;
compute_game_money({betli, piros}) ->
  100;
compute_game_money({durtmars}) ->
  60;
compute_game_money({durtmars, piros}) ->
  60;
compute_game_money({szinnelkuli_durtmars}) ->
  120;
compute_game_money({teritett_durtmars}) ->
  120;
compute_game_money({teritett_durtmars, piros}) ->
  240;
compute_game_money({teritett_szinnelkuli_durtmars}) ->
  240;
compute_game_money({teritett_betli}) ->
  200.

