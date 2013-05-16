-module (flokk_middleware_pubsub).

-export([execute/2]).

execute(Req, Env) ->
  %% TODO publish change events to a pubsub server
  {ok, Req, Env}.
