-module(flokk_hook).

-export([start/1]).
-export([terminate/4]).

start(Req) ->
  Req.

terminate(Status, Headers, _Body, Req) ->
  error_body(Status, Headers, Req).

error_body(404, _Headers, Req) ->
  Body = jsx:encode([
    {<<"error">>, <<"not_found">>}
  ]),
  cowboy_req:set_resp_body(Body, Req);
error_body(_, _, Req) ->
  Req.
