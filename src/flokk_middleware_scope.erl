-module (flokk_middleware_scope).

-export([execute/2]).

execute(Req, Env) ->
  Opts = proplists:get_value(handler_opts, Env), %% Try using flokk_util later - was getting the key/value tuple here
  Scope = proplists:get_value(scope, Opts),
  {AccessToken, Req} = cowboy_req:meta(access_token, Req),

  case Scope of
    undefined ->
      {ok, Req, Env};
    Scope when AccessToken =:= undefined ->
      %% TODO configure the realm
      Req1 = cowboy_req:set_resp_header(<<"www-authenticate">>, <<"Bearer realm=\"https://auth.theflokk.com\"">>, Req),
      {error, 401, Req1};
    Scope ->
      {UserScopes, Req} = cowboy_req:meta(scopes, Req, []),
      case lists:member(Scope, UserScopes) of
        true -> {ok, Req, Env};
        _ -> {error, 403, Req}
      end
  end.
