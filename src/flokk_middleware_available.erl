-module (flokk_middleware_available).

-export([execute/2]).

execute(Req, Env) ->
  Opts = proplists:get_value(handler_opts, Env),
  case proplists:get_value(service, Opts) of
    undefined ->
      {ok, Req, Env};
    Service ->
      case Service:available() of
        true -> {ok, Req, Env};
        _ -> {error, 503, Req}
      end
  end.
