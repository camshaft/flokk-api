-module (flokk_middleware_etag).

-export([execute/2]).

execute(Req, Env) ->
  case cowboy_req:meta(body, Req) of
    {undefined, Req} ->
      {ok, Req, Env};
    {Body, Req} ->
      Hash = list_to_binary(integer_to_list(erlang:phash2(Body))),
      %% TODO look at the etag they sent us and do a check on it
      Req2 = cowboy_req:set_resp_header(<<"etag">>, <<"W/",Hash/binary>>, Req),
      {ok, Req2, Env}
  end.
