{application,ulti_game,
             [{description,"Ulti game application"},
              {id,"ulti_game"},
              {vsn,"0.1.0"},
              {registered,[ulti_room_server]},
              {applications,[kernel,stdlib,crypto,sasl]},
              {mod,{ulti_game,[]}},
              {modules,[ulti_app,ulti_eval,ulti_game,ulti_game_play]}]}.
