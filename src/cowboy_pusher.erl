-module (cowboy_pusher).

-export([execute/2]).

execute(Req, Env) ->
  Req2 = publish(cowboy_req:meta(pub_event, Req)),
  {ok, Req2, Env}.

publish({{Event, Data}, Req}) ->
  {URL, Req2} = cowboy_req:url(Req),
  Channel = websaferl:encode(URL),
  pusherl:push_async(Channel, Event, Data),
  Req2;
publish({Event, Req}) when is_binary(Event) ->
  {URL, Req2} = cowboy_req:url(Req),
  Channel = websaferl:encode(URL),
  pusherl:push_async(Channel, Event, <<>>),
  Req2;
publish({_, Req}) ->
  Req.
