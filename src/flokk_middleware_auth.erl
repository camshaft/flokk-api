-module (flokk_middleware_auth).

-export([execute/2]).

execute(Req, Env) ->
  {Authorization, Req} = cowboy_req:header(<<"authorization">>, Req),
  AccessToken = access_token(Authorization),
  Req1 = cowboy_req:set_meta(access_token, AccessToken, Req),
  %% TODO decrypt the access token info and get the user scopes
  %% User = simple_secrets:unpack(AccessToken, MasterKey),
  User = [],
  UserId = flokk_util:get_value(<<"i">>, User),
  Scopes = flokk_util:get_value(<<"s">>, User, []),
  Req2 = cowboy_req:set_meta(user_id, UserId, Req1),
  Req3 = cowboy_req:set_meta(scopes, Scopes, Req2),
  {ok, Req3, Env}.

access_token(<<"Bearer ",AccessToken/binary>>) -> AccessToken;
access_token(<<"bearer ",AccessToken/binary>>) -> AccessToken;
access_token(_) -> undefined.
