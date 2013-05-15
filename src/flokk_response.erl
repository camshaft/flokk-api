-module (flokk_response).

-export([handle/4]).

handle(Status, _Headers, _Body, Req) ->
  {Method, Req} = cowboy_req:method(Req),
  {Path, Req} = cowboy_req:path(Req),
  lager:debug("~s ~s ~b~n", [binary_to_list(Method), binary_to_list(Path), Status]),
  Req.
