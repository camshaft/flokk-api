-module (flokk_middleware_auth).

-export([execute/2]).

execute(Req, Env) ->
  {Authorization, Req} = cowboy_req:header(<<"authorization">>, Req),

  case access_token(Authorization) of
    undefined ->
      {ok, Req, Env};
    AccessToken ->
      Req1 = cowboy_req:set_meta(access_token, AccessToken, Req),
      Sender = proplists:get_value(sender, Env),
      Data = simple_secrets:unpack(AccessToken, Sender),
      {UserId, Scopes} = parse_data(Data),
      Req2 = cowboy_req:set_meta(user_id, UserId, Req1),
      Req3 = cowboy_req:set_meta(scopes, Scopes, Req2),
      {ok, Req3, Env}
  end.

access_token(<<"Bearer ",AccessToken/binary>>) -> AccessToken;
access_token(<<"bearer ",AccessToken/binary>>) -> AccessToken;
access_token(_) -> undefined.

parse_data(Data)->
  User = cowboy_http:params(Data, fun (_Rest, Properties) ->
    Properties
  end),

  Scopes = cowboy_http:list(proplists:get_value(<<"s">>, User, <<"">>), fun (_, Properties) ->
    Properties
  end),

  UserId = proplists:get_value(<<"i">>, User),

  %% TODO parse the expiration date and make sure it's still valid

  {UserId, Scopes}.
