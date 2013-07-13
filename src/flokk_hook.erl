-module (flokk_hook).

-export([start/1]).
-export([terminate/4]).

start(Req) ->
  Req.

terminate(Status, Headers, _Body, Req) ->
  Req2 = error_body(Status, Headers, Req),
  publish(cowboy_req:meta(pub_event, Req2)).

error_body(404, _Headers, Req) ->
  Body = jsx:encode([
    {<<"error">>, <<"notfound">>}
  ]),
  cowboy_req:set_resp_body(Body, Req);
error_body(_, _, Req) ->
  Req.

publish({{Event, Data}, Req}) ->
  {URL, Req2} = cowboy_req:url(Req),
  pusherl:push(URL, Event, Data),
  Req2;
publish({Event, Req}) when is_binary(Event) ->
  {URL, Req2} = cowboy_req:url(Req),
  pusherl:push(URL, Event, <<>>),
  Req2;
publish({_, Req}) ->
  Req.
