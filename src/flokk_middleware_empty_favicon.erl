-module (flokk_middleware_empty_favicon).

-export([execute/2]).

execute(Req, Env) ->
  {Path, Req} = cowboy_req:path(Req),
  case Path of
    <<"/favicon.ico">> -> {halt, Req};
    _ -> {ok, Req, Env}
  end.
