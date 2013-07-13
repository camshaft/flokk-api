-module (cowboy_pusher).

-export([execute/2]).

execute(Req, Env) ->
  {Path, Req2} = cowboy_req:path(Req),
  {QS, Req3} = cowboy_req:qs(Req2),
  URL = case QS of
    <<>> -> Path;
    QS -> <<Path/binary,"?",QS/binary>>
  end,
  Req4 = publish(cowboy_base:resolve(URL, Req3), cowboy_req:meta(pub_event, Req3)),
  {ok, Req4, Env}.

publish(URL, {{Event, Data}, Req}) ->
  Channel = websaferl:encode(URL),
  pusherl:push_async(Channel, Event, Data),
  Req;
publish(URL, {Event, Req}) when is_binary(Event) ->
  Channel = websaferl:encode(URL),
  pusherl:push_async(Channel, Event, <<>>),
  Req;
publish(_, {_, Req}) ->
  Req.
