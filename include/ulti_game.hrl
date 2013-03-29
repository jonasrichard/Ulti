%% Copyright
-author("Richard_Jonas").

-record(game, {
  color,         %% during licit: undefined during licit or piros, during game can be any color
  types,         %% list of game types (ulti, betli, etc.)
  options        %% list of kontra and game type pairs: {kontra, party}
}).
