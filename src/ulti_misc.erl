%% Copyright
-module(ulti_misc).
-author("Richard_Jonas").

-include("ulti_game.hrl").

%% API
-export([deal/0, sort/1,
  which_player_take/2, value_to_number/1]).

-spec deal() -> {hand(), hand(), hand()}.
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

-spec generate_cards() -> [card()].
generate_cards() ->
  [{X,Y} || X <- [tok, makk, piros, zold], Y <- [7, 8, 9, also, felso, kiraly, 10, asz]].

-spec color_to_number(color()) -> 1..4.
color_to_number(tok) ->
  1;
color_to_number(makk) ->
  2;
color_to_number(zold) ->
  3;
color_to_number(piros) ->
  4.

-spec value_to_number(face()) -> 1..8.
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

-spec compare_cards(card(), card()) -> boolean().
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

-spec sort([card()]) -> [card()].
sort(Cards) ->
  lists:sort(fun compare_cards/2, Cards).

%%
%% Gives which card hit the cards on the table
%%
-spec which_player_take(ThreeCard, BeatFun) -> {number(), card()} when
  ThreeCard  :: [card()],
  BeatFun    :: fun((Card::card(), Other::card()) -> boolean()).
which_player_take([Card1, Card2, Card3], BeatFun) ->
  which_player_take(Card1, [Card2, Card3], BeatFun).
which_player_take({_N, _C} = Card, [], _BeatFun) ->
  Card;
which_player_take({N, Card}, [{No, OtherCard} | T], BeanFun) ->
  case BeanFun(Card, OtherCard) of
    true ->
      which_player_take({N, Card}, T, BeanFun);
    _ ->
      which_player_take({No, OtherCard}, T, BeanFun)
  end.

%%
%% Validate game combinations
%%
is_valid_game({Color, Games} = Game) ->
  (Color == undefined orelse color_to_number(Color))
    andalso
  (is_valid_simple_game(Games) orelse (length(Games) > 0 andalso is_valid_combined_game(Games)))
    andalso
  case Game of
    {piros, [husz_szaz]} -> false;
    _ -> true
  end.

is_valid_simple_game([passz]) ->
  true;
is_valid_simple_game([betli]) ->
  true;
is_valid_simple_game([rebetli]) ->
  true;
is_valid_simple_game([teritett_betli]) ->
  true;
is_valid_simple_game([szintelen_durchmars]) ->
  true;
is_valid_simple_game([redurchmars]) ->
  true;
is_valid_simple_game([teritett_szintelen_durchmars]) ->
  true;
is_valid_simple_game(_) ->
  false.

is_valid_combined_game([]) ->
  false;
is_valid_combined_game([Game|T] = Games) ->
  lists:all(fun(G) -> not is_valid_simple_game([G]) end, Games)
    andalso
  case lists:member(negy_asz, Games) of
    false ->
      true;
    _ ->
      %%lists:all(fun(G) -> G /= durchmars or G /= teritett_durchmars end, Games)
      error
  end
    andalso
  case lists:member(negyven_szaz, Games) of
    false ->
      true;
    _ ->
      not lists:member(husz_szaz, Games)
  end
    andalso
  case lists:member(husz_szaz, Games) of
    false ->
      true;
    _ ->
      not lists:member(negyven_szaz, Games)
  end.
