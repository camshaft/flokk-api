-module (flokk_middleware_request_id).

-export([execute/2]).

execute(Req, Env) ->
  Req2 = case cowboy_req:header(<<"x-request-id">>, Req) of
    {undefined, Req} ->
      Req;
    {RequestID, Req} ->
      cowboy_req:set_meta(request_id, RequestID, Req)
  end,
  {ok, Req2, Env}.
