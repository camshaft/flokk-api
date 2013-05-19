-module (flokk_hook).

-export([start/1]).
-export([terminate/4]).

start(Req) ->
  lager:debug("------- start -------"),
  cowboy_req:set_meta(start_time, now(), Req).

terminate(Status, Headers, _Body, Req) ->
  Req2 = error_body(Status, Headers, Req),
  [Method, Path] = cowboy_req:get([method, path], Req2),
  {Start, Req2} = cowboy_req:meta(start_time, Req2),
  lager:debug("~s ~s ~b ~pus~n----- terminate -----", [binary_to_list(Method), binary_to_list(Path), Status, timer:now_diff(now(), Start)]),
  Req2.

error_body(404, _Headers, Req) ->
  Body = jsx:encode([
    {<<"error">>, <<"notfound">>}
  ]),
  cowboy_req:set_resp_body(Body, Req);
error_body(_, _, Req) ->
  Req.
