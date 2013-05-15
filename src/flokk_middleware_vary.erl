-module (flokk_middleware_vary).

-export([execute/2]).

execute(Req, Env) ->
  Req1 = cowboy_req:set_resp_header(<<"vary">>, <<"authorization">>, Req),
  {ok, Req1, Env}.
