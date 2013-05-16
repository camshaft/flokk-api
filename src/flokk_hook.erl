-module (flokk_hook).

-export([handle/4]).

%% TODO have generic error responses

handle(Status, _Headers, _Body, Req) ->
  {<<"/",Path/binary>>, Req} = cowboy_req:path(Req),
  lager:debug("~s ~b~n", [binary_to_list(Path), Status]),
  Req.
