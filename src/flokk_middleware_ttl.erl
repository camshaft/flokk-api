-module (flokk_middleware_ttl).

-export([execute/2]).

%% TODO only encode if the user accepts json
%% TODO do we need to even check? how many media types do we even need to support?
execute(Req, Env) ->
  case cowboy_req:meta(ttl, Req) of
    {undefined, Req} ->
      {ok, Req, Env};
    {TTL, Req} ->
      Req2 = cowboy_req:set_resp_header(<<"cache-control">>, <<"private, max-age=", TTL/binary>>, Req),
      {ok, Req2, Env}
  end.
