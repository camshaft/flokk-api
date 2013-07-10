-module (flokk_hook).

-export([start/1]).
-export([terminate/4]).

start(Req) ->
  % io:format("~p~n",[cowboy_req:headers(Req)]),
  Req.

terminate(Status, Headers, _Body, Req) ->
  Req2 = error_body(Status, Headers, Req),
  Req2.

error_body(404, _Headers, Req) ->
  Body = jsx:encode([
    {<<"error">>, <<"notfound">>}
  ]),
  cowboy_req:set_resp_body(Body, Req);
error_body(_, _, Req) ->
  Req.
