%% Copyright
-author("Richard_Jonas").

-record(game, {
        color    :: color(), 
            %% during licit: undefined during licit or piros,
            %% during game can be any color
        types    :: game_type(),
            %% list of game types (ulti, betli, etc.)
        options
            %% list of kontra and game type pairs: {kontra, party}
}).

-type color()         :: piros | tok | zold | makk.
-type face()          :: 7 | 8 | 9 | also | felso | kiraly | 10 | asz.
-type card()          :: {color(), face()}.

-type game_type()     :: party | negyven_szaz | negy_asz | ulti | betli |
                         durchmars | szintelen_durchmars | husz_szaz |
                         rebetli | redurchmars | teritett_betli |
                         teritett_durchmars.

-type licit()         :: {game_type()} | {game_type(), piros}.

-type kontra()        :: {kontra | rekontra | szubkontra | mordkontra, 
                          pid()}.

-type game()          :: {game_type(), face(), [kontra()]}.


-type player()        :: {Name::string(), EventHandler::pid()}.
-type hand()          :: [card()].

-type card_put()      :: {pid(), card()}.

-type bela()          :: {husz, 1..3} | negyven.
-type take()          :: {Round::1..10, [card_put()]}.

