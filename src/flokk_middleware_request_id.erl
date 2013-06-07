-module (flokk_middleware_request_id).

-export([execute/2]).

execute(Req, Env) ->
  lager:debug("middleware:request_id"),
  Req2 = case cowboy_req:header(<<"x-request-id">>, Req) of
    {undefined, Req} ->
      case cowboy_req:header(<<"heroku-request-id">>, Req) of
        {undefined, Req} ->
          Req;
        {RequestID, Req} ->
          cowboy_req:set_resp_header(<<"x-request-id">>, RequestID, Req)
      end;
    {RequestID, Req} ->
      cowboy_req:set_resp_header(<<"x-request-id">>, RequestID, Req)
  end,
  {ok, Req2, Env}.
