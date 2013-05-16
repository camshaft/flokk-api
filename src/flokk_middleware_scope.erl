-module (flokk_middleware_scope).

-export([execute/2]).

execute(Req, Env) ->
  Opts = proplists:get_value(handler_opts, Env), %% Try using flokk_util later - was getting the key/value tuple here
  Scope = flokk_util:get_value(scope, Opts),

  case Scope of
    undefined ->
      {ok, Req, Env};
    Scope ->
      {UserScopes, Req} = cowboy_req:meta(scopes, Req),
      case lists:member(Scope, UserScopes) of
        true ->
          {ok, Req, Env};
        false ->
          {ok, Req2} = cowboy_req:reply(403, Req),
          %% TODO send back a better error response
          {halt, Req2, Env}
      end
  end.
