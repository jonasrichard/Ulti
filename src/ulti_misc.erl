%% Copyright
-module(ulti_misc).
-author("Richard_Jonas").

-include("ulti_game.hrl").

%% API
-export([deal/0, sort/1,
  face_to_number/1, next_kontra/1,
  which_player_take/2, beats/2, beats_low10/2, beats_trump/3]).

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

-spec face_to_number(face()) -> 1..8.
face_to_number(7) ->
  1;
face_to_number(8) ->
  2;
face_to_number(9) ->
  3;
face_to_number(also) ->
  4;
face_to_number(felso) ->
  5;
face_to_number(kiraly) ->
  6;
face_to_number(10) ->
  7;
face_to_number(asz) ->
  8.

-spec face_to_number_low10(face()) -> 1..8.
face_to_number_low10(7) ->
  1;
face_to_number_low10(8) ->
  2;
face_to_number_low10(9) ->
  3;
face_to_number_low10(10) ->
  4;
face_to_number_low10(also) ->
  5;
face_to_number_low10(felso) ->
  6;
face_to_number_low10(kiraly) ->
  7;
face_to_number_low10(asz) ->
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
        face_to_number(Value1) =< face_to_number(Value2)
      end
  end.

-spec sort([card()]) -> [card()].
sort(Cards) ->
  lists:sort(fun compare_cards/2, Cards).

%% Card2 beats Card1
-spec beats(card(), card()) -> boolean().
beats({Color1, Face1} = _Card1, Card2) ->
  case Card2 of
    {Color1, Face2} ->
      face_to_number(Face2) > face_to_number(Face1);
    _ ->
      false
  end.

-spec beats_low10(card(), card()) -> boolean().
beats_low10({Color1, Face1} = _Card1, Card2) ->
  case Card2 of
    {Color1, Face2} ->
      face_to_number_low10(Face2) > face_to_number_low10(Face1);
    _ ->
      false
  end.

-spec beats_trump(card(), card(), face()) -> boolean().
beats_trump(Card1, Card2, Trump) ->
  case {Card1, Card2} of
    {{Trump, Face1}, {Trump, Face2}} ->
      face_to_number(Face2) > face_to_number(Face1);
    {{Trump, _}, _} ->
      false;
    {_, {Trump, _}} ->
      true;
    {_, _} ->
      beats(Card1, Card2)
  end.

-spec which_player_take([{any, card()}], Option) -> {any(), card()} when
  Option :: nil | low10 | {trump, face()}.
which_player_take([H | T], Option) ->
  which_player_take(H, T, Option).
which_player_take(TopCard, [], _Option) ->
  TopCard;
which_player_take({N1, TopCard}, [{N2, Card} | Others], Option) ->
  NewTop =
    case Option of
      nil ->
        case beats(TopCard, Card) of
          true ->
            {N2, Card};
          false ->
            {N1, TopCard}
        end;
      low10 ->
        case beats_low10(TopCard, Card) of
          true ->
            {N2, Card};
          false ->
            {N1, TopCard}
        end;
      {trump, Trump} ->
        case beats_trump(TopCard, Card, Trump) of
          true ->
            {N2, Card};
          false ->
            {N1, TopCard}
        end
    end,

  which_player_take(NewTop, Others, Option).


next_kontra(kontra) -> rekontra;
next_kontra(rekontra) -> szubkontra;
next_kontra(szubkontra) -> mordkontra.

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
