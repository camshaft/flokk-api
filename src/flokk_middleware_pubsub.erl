-module (flokk_middleware_pubsub).

-export([execute/2]).

execute(Req, Env) ->
  lager:debug("middleware:pubsub"),
  %% TODO publish change events to a pubsub server
  {ok, Req, Env}.
