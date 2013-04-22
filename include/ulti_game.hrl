%% Copyright
-author("Richard_Jonas").

-record(game, {
  color,         %% during licit: undefined during licit or piros, during game can be any color
  types,         %% list of game types (ulti, betli, etc.)
  options        %% list of kontra and game type pairs: {kontra, party}
}).

-type color()    :: piros | tok | zold | makk.
-type face()     :: 7 | 8 | 9 | also | felso | kiraly | 10 | asz.
-type card()     :: {color(), face()}.

-type player()   :: {Name::string(), HttpHandler::pid()}.
-type hand()     :: [card()].

-type card_put() :: {PlayerNo::1..3, card()}.
-type take()     :: {Round::1..10, [card_put()]}.
