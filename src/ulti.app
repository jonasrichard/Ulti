
{application, ulti, [
  {description, "Ulti server"},
  {id, "Ulti"},
  {vsn, "0.0.1"},
  {applications, [crypto, ranch, cowboy]},
  {registered, [ulti_room_server]},
  {mod, {ulti_app, []}}
]}.
